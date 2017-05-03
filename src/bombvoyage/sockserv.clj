(ns bombvoyage.sockserv
  (:require [org.httpkit.server :refer [run-server]]
            [chord.http-kit :refer [with-channel]]
            [clojure.core.async :refer [<! >! go-loop timeout go alts! chan close!]
                                :as a]
            [clojure.core.match :refer [match]]
            [compojure.handler :refer [site]]
            [clojure.set]
            [clj-jwt.core :refer :all]
            [compojure.route :as route]
            [compojure.core :refer :all]
            [bombvoyage.specs :as s]))

(def players (atom 0))
(def lobby-tokens (atom {}))
(def ^:dynamic *config* nil)

(defn player-middleware
  "Adds middleware that adds id to payload and emits close events."
  [pid pchan]
  (let [pchan-out (chan)]
    (go-loop []
      (if-let [msg (:message (<! pchan))]
        (do
          (>! pchan-out [:message pid msg])
          (recur))
        (do
          (>! pchan-out [:close pid])
          (close! pchan-out))))
    pchan-out))

(defn subscribe
  "Subscribes the player to the games input/output feed."
  [pid pchan [mix mult]]
  (a/admix mix (player-middleware pid pchan))
  (a/tap mult pchan))

(defn submit-tick [in state-type]
  "Submits a tick to the game after a delay."
  (let [tick (get-in s/specs [state-type :tick-len])]
    (go
      (<! (timeout tick))
      (>! in [:tick state-type]))))

(defn get-fn [state func] (get-in s/specs [(:type state) func]))
(defn tick-len [state] (get-fn state :tick-len))
(defn max-players [state] (get-fn state :max-players))
(defn tick-fn [state] ((get-fn state :tick-fn) state))
(defn complete? [state] ((get-fn state :complete?) state))
(defn join-fn [state pid] ((get-fn state :join-fn) state pid))
(defn leave-fn [state pid] ((get-fn state :leave-fn) state pid))
(defn action-fn [state pid action]
  ((get-fn state :action-fn) state pid action))
(defn transition-state [state in players]
  (let [next-type (get-fn state :next-spec)
        next-init (get-in s/specs [next-type :init-fn])
        next-state (next-init players state)]
    (submit-tick in next-type)
    next-state))

(defn lobby-loop
  "Starts the game loop with init-state & players"
  [init-state init-players in out sub]
  (go-loop [state init-state players init-players]
    (when-not (empty? players)
      (>! out [:set-state state])
      (if (complete? state)
        (recur (transition-state state in players) players)
        (match (<! in)
          [:join pid pchan]
             (if (< (count players) (max-players state))
               (do (subscribe pid pchan sub)
                   (recur (join-fn state pid) (conj players pid)))
               (do (close! pchan)
                   (recur state players)))
          [:close pid]
             (recur
               (leave-fn state pid)
               (clojure.set/difference players #{pid}))
          [:tick state-type]
             (if (= state-type (:type state))
               (do
                 (submit-tick in (:type state))
                 (recur (tick-fn state) players))
               (recur state players))
          [:message pid body]
             (recur (action-fn state pid body) players))))))

(defn run-lobby-loop [init-pid init-pchan completion-fn]
  "Runs the game loop, starts the ticks and cleans up after"
  (let [in (chan)
        out (chan)
        sub [(a/mix in) (a/mult out)]
        init-state ((:init-fn s/init-spec) #{init-pid} nil)]
    (go
      (subscribe init-pid init-pchan sub)
      (<! (lobby-loop init-state #{init-pid} in out sub))
      (close! in)
      (close! out)
      (completion-fn))
    (submit-tick in (:type init-state))
    in))

(defn make-lobby [lobby-id pid pchan]
  "Makes a game and joins the player to the game"
  (let [completion-fn #(swap! lobby-tokens dissoc lobby-id)
        lobby-in (run-lobby-loop pid pchan completion-fn)]
    (swap! lobby-tokens assoc lobby-id lobby-in)))

(defn sock-handler
  "Handles incoming websockets."
  [req]
  (with-channel req ws-ch
    (go []
      (let [player-id (swap! players inc)
            encoded (str->jwt (:message (<! ws-ch)))
            [act-str lobby-id] (:game-token (:claims encoded))
            lobby-in (@lobby-tokens lobby-id)
            action (keyword act-str)]
        (println (:claims encoded))
        (>! ws-ch [:set-id player-id])
        (cond
          (not (verify encoded (:secret *config*))) (close! ws-ch)
          (= action :create) (make-lobby lobby-id player-id ws-ch)
          (and (= action :join) lobby-in)
            (>! lobby-in [:join player-id ws-ch]))))))

(defn run [config]
  (alter-var-root (var *config*) (fn [_] config))
  (str->jwt (to-str (jwt {:a [:create 1]})))
  (run-server
    (routes
      (GET "/" []
             (let [tokens @lobby-tokens]
               (str (zipmap (keys tokens) (repeat (:me *config*))))))
      (GET "/ws" [] sock-handler)
      (route/resources "/")
      (route/not-found "<h1>Page not found</h1>"))
    {:port (:port config)}))

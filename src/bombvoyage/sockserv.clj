(ns bombvoyage.sockserv
  (:require [org.httpkit.server :refer [run-server]]
            [chord.http-kit :refer [with-channel]]
            [clojure.core.async :refer [<! >! go-loop timeout go alts! chan close!]
                                :as a]
            [clojure.core.match :refer [match]]
            [compojure.handler :refer [site]]
            [clojure.set :as s]
            [clj-jwt.core :refer :all]
            [compojure.route :as route]
            [compojure.core :refer :all]
            [bombvoyage.lobby :as l]
            [bombvoyage.game :as g]))

(def players (atom 0))
(def lobby-tokens (atom {}))
(def init-chat {:type :chat :ticks-left 10 :players #{}})
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

(defn submit-tick [in tick]
  "Submits a tick to the game after a delay."
  (go
    (<! (timeout tick))
    (>! in [:tick])))

(defn lobby-loop [init-state in out sub]
  "Starts the game loop with the init-state"
  (go-loop [state init-state players #{}]
    (match (<! in)
      [:join pid pchan]
          (if-let [nex (l/join-game state pid)]
            (do (subscribe pid pchan sub)
                (>! out [:set-state nex])
                (recur nex (conj players pid)))
            (do (close! pchan)
                (recur state players)))
      [:close pid]
          (let [nex (l/leave-game state pid)
                nex-players (s/difference players #{pid})]
            (>! out [:set-state nex])
            (when-not (empty? nex-players)
              (recur nex nex-players)))
      [:tick]
          (let [nex (l/tick state)]
            (>! out [:set-state nex])
            (submit-tick in (l/tick-len nex))
            (recur nex players))
      [:message pid body]
          (let [nex (l/on-action state pid body)]
            (>! out [:set-state nex])
            (recur nex players)))))

(defn run-lobby-loop [init-state completion-fn]
  "Runs the game loop, starts the ticks and cleans up after"
  (let [in (chan)
        out (chan)
        sub [(a/mix in) (a/mult out)]]
    (go
      (<! (lobby-loop init-state in out sub))
      (close! in)
      (close! out)
      (completion-fn))
    (submit-tick in (l/tick-len init-state))
    in))

(defn make-lobby [lobby-id pid pchan]
  "Makes a game and joins the player to the game"
  (let [completion-fn #(swap! lobby-tokens dissoc lobby-id)
        lobby-in (run-lobby-loop init-chat completion-fn)]
    (swap! lobby-tokens assoc lobby-id lobby-in)
    (go (>! lobby-in [:join pid pchan]))))

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

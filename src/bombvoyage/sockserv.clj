(ns bombvoyage.sockserv
  (:require [org.httpkit.server :refer [run-server]]
            [chord.http-kit :refer [with-channel]]
            [clojure.core.async :refer [<! >! go-loop timeout go alts! chan close!]
                                :as a]
            [clojure.core.match :refer [match]]
            [compojure.handler :refer [site]]
            [compojure.route :as route]
            [compojure.core :refer :all]
            [bombvoyage.lobby :as l]
            [bombvoyage.game :as g])
  (:gen-class))

(def me "localhost:8081")
(def players (atom 0))
(def game-tokens (atom {}))

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

(defn handle-action
  "Handles an action coming across the wire from a player."
  [pid game action]
  (match action
    [:drop-bomb] (g/drop-bomb game pid)
    [:start-moving dir] (g/start-moving game pid dir)
    [:stop-moving] (g/stop-moving game pid)))

(defn start-game-loop
  "Starts the game loop."
  [game-id init-state in out game-sub]
  (go-loop [state init-state]
    (let [[msg _] (alts! [in (timeout g/TICK-LEN)])]
      (match msg
        [:join pid pchan]
          (if (< (count (:players state)) g/MAX-PLAYERS)
            (do
              (subscribe pid pchan game-sub)
              (let [nex (g/add-player state pid)]
                (>! out [:set-state nex])
                (recur nex)))
            (do
              (close! pchan)
              (recur state)))
        [:close pid]
          (let [nex (g/remove-player state pid)]
            (>! out [:set-state nex])
            (if (zero? (count (:players nex)))
              (swap! game-tokens dissoc game-id)
              (recur nex)))
        [:message pid body]
          (let [nex (handle-action pid state body)]
            (>! out [:set-state nex])
            (recur nex))
        :else
          (let [nex (g/tick state)]
            (>! out [:set-state nex])
            (recur nex))))))

(defn make-game
  "Makes a game and starts the game loop for that game."
  [game-id init-player-id init-player-chan]
  (let [game-chan-in (chan)
        game-chan-out (chan)
        game-sub [(a/mix game-chan-in) (a/mult game-chan-out)]
        init-state
          (-> (g/rand-game)
              (g/add-player init-player-id))]
    (swap! game-tokens assoc game-id game-chan-in)
    (subscribe init-player-id init-player-chan game-sub)
    (start-game-loop
      game-id init-state game-chan-in game-chan-out game-sub)))

(defn sock-handler
  "Handles incoming websockets."
  [req]
  (with-channel req ws-ch
    (go []
      (let [player-id (swap! players inc)]
        (>! ws-ch [:set-id player-id])
        (match (:message (<! ws-ch))
          [:create game-id]
               (make-game game-id player-id ws-ch)
          [:join game-id]
               (if-let [game-in (@game-tokens game-id)]
                 (>! game-in [:join player-id ws-ch])))))))

(defn run []
  (run-server
    (routes
      (GET "/" []
             (let [tokens @game-tokens]
               (str (zipmap (keys tokens) (repeat me)))))
      (GET "/ws" [] sock-handler)
      (route/resources "/")
      (route/not-found "<h1>Page not found</h1>"))
    {:port 8081}))

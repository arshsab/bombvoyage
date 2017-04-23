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

(defn make-game [game-id pid pchan]
  (let [completion-fn #(swap! game-tokens dissoc game-id)
        game-in (l/run-game-loop (g/rand-game) completion-fn)]
    (swap! game-tokens assoc game-id game-in)
    (go (>! game-in [:join pid pchan]))))

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

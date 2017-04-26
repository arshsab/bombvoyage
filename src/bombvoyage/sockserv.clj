(ns bombvoyage.sockserv
  (:require [org.httpkit.server :refer [run-server]]
            [chord.http-kit :refer [with-channel]]
            [clojure.core.async :refer [<! >! go-loop timeout go alts! chan close!]
                                :as a]
            [clojure.core.match :refer [match]]
            [compojure.handler :refer [site]]
            [clj-jwt.core :refer :all]
            [compojure.route :as route]
            [compojure.core :refer :all]
            [bombvoyage.lobby :as l]
            [bombvoyage.game :as g]))

(def players (atom 0))
(def game-tokens (atom {}))
(def init-chat {:type :chat :ticks-left 10 :players #{}})
(def ^:dynamic *config* nil)

(defn make-game [game-id pid pchan]
  (let [completion-fn #(swap! game-tokens dissoc game-id)
        game-in (l/run-game-loop init-chat completion-fn)]
    (swap! game-tokens assoc game-id game-in)
    (go (>! game-in [:join pid pchan]))))

(defn sock-handler
  "Handles incoming websockets."
  [req]
  (with-channel req ws-ch
    (go []
      (let [player-id (swap! players inc)]
        (>! ws-ch [:set-id player-id])
        (let [payload (:message (<! ws-ch))
              decoded (str->jwt payload)]
          (println (:claims decoded))
          (println (verify decoded (:secret *config*)))
          (if (verify decoded (:secret *config*))
            (match (get-in decoded [:claims :game-token])
              ["create" game-id]
                   (make-game game-id player-id ws-ch)
              ["join" game-id]
                   (if-let [game-in (@game-tokens game-id)]
                     (>! game-in [:join player-id ws-ch]))
              :else
                   (println (:claims decoded)))))))))

(defn run [config]
  (alter-var-root (var *config*) (fn [_] config))
  (run-server
    (routes
      (GET "/" []
             (let [tokens @game-tokens]
               (str (zipmap (keys tokens) (repeat (:me *config*))))))
      (GET "/ws" [] sock-handler)
      (route/resources "/")
      (route/not-found "<h1>Page not found</h1>"))
    {:port (:port config)}))

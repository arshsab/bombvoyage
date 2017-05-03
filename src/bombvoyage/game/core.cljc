(ns bombvoyage.game.core
  (:require #?(:clj  [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]])
            [bombvoyage.game.logic :as l]))

(defn identity2 [x & _] x)

(defn game-on-action [game pid action]
  (if (get-in game [:players pid :alive?])
    (match action
      [:drop-bomb] (l/drop-bomb game pid)
      [:start-moving dir] (l/start-moving game pid dir)
      [:stop-moving] (l/stop-moving game pid)
      :else game)
    game))

(def spec
  {:tick-len 30
   :init-fn (fn [player-ids _] (l/rand-game player-ids))
   :tick-fn l/tick
   :max-players 4
   :join-fn identity2
   :leave-fn l/remove-player
   :complete? l/game-over?
   :action-fn game-on-action
   :next-spec :game-over})

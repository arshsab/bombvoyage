(ns bombvoyage.gameover)

(defn identity2 [x & _] x)

(def spec
  {:tick-len 6000
   :init-fn
      (fn [player-ids last-state]
        {:type :game-over
         :players player-ids
         :last-state last-state
         :ticked? false})
   :tick-fn #(assoc % :ticked? true)
   :max-players 4
   :join-fn identity2
   :leave-fn identity2
   :action-fn identity2
   :complete? :ticked?
   :next-spec :chat})

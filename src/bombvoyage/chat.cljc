(ns bombvoyage.chat
  (:require #?(:clj  [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]])
            [clojure.set :as s]))

(defn identity2 [x & _] x)

(defn chat-tick [state]
  (if (> (:players state) 1)
    (update state :ticks-left dec)
    state))

(defn chat-join [state pid]
  (let [nex-state (update state :players inc)]
    (if (= 1 (:players state))
      (assoc nex-state :ticks-left 20)
      nex-state)))

(defn chat-leave [state pid]
  (let [nex-state (update state :players dec)]
    (if (= 1 (:players nex-state))
      (assoc nex-state :ticks-left 20)
      nex-state)))

(defn chat-on-action [state pid action]
  (match action
    [:post message]
       (->> (:messages state)
            (cons message)
            (take 10)
            (assoc state :messages))
    :else state))

(defn chat-over? [state]
  (zero? (:ticks-left state)))

(def spec
  {:tick-len 1000
   :init-fn
      (fn [pids _]
        {:type :chat
         :players (count pids)
         :messages []
         :ticks-left 20})
   :tick-fn chat-tick
   :max-players 4
   :join-fn chat-join
   :leave-fn chat-leave
   :action-fn chat-on-action
   :complete? chat-over?
   :next-spec :game})

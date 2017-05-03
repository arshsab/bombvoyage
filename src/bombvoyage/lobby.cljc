(ns bombvoyage.lobby
  (:require #?(:clj  [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]])
            [bombvoyage.game :as g]
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
  (let [nex-state (update state :players inc)]
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

(defn game-on-action [game pid action]
  (match action
    [:drop-bomb] (g/drop-bomb game pid)
    [:start-moving dir] (g/start-moving game pid dir)
    [:stop-moving] (g/stop-moving game pid)
    :else game))

(def game-spec
  {:tick-len 30
   :init-fn (fn [player-ids _] (g/rand-game player-ids))
   :tick-fn g/tick
   :max-players 4
   :join-fn identity2
   :leave-fn g/remove-player
   :complete? g/game-over?
   :action-fn game-on-action
   :next-spec :game-over})

(def game-over-spec
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

(def chat-spec
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

(def specs {:game game-spec
            :game-over game-over-spec
            :chat chat-spec})
(def init-spec chat-spec)

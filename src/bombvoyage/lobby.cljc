(ns bombvoyage.lobby
  (:require #?(:clj  [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]])
            [bombvoyage.game :as g]
            [clojure.set :as s]))

(def game-spec
  {:tick-len 30
   :tick-fn g/tick
   :max-players 4
   :join-fn (constantly nil)
   :leave-fn g/remove-player
   :complete? g/game-over?
   :next-state :game-over})

(defn game-type [state & _] (:type state))

(defmulti join-game game-type)
(defmulti tick-len game-type)
(defmulti tick game-type)
(defmulti on-action game-type)
(defmulti leave-game game-type)

(def CHAT-LEN 20)
(defmethod join-game :game [game pid] nil)
(defmethod tick-len :game [_] g/TICK-LEN)
(defmethod tick :game [game]
  (let [nex (g/tick game)]
    (if (g/game-over? nex)
      {:type :chat
       :ticks-left CHAT-LEN
       :players (set (keys (:players game)))}
      nex)))
(defmethod on-action :game [game pid action]
  (match action
    [:drop-bomb] (g/drop-bomb game pid)
    [:start-moving dir] (g/start-moving game pid dir)
    [:stop-moving] (g/stop-moving game pid)))
(defmethod leave-game :game [game pid]
  (g/remove-player game pid))

(defmethod join-game :chat [chat pid]
  (let [players (count (:players chat))
        added (update chat :players conj pid)
        reset (assoc added :ticks-left CHAT-LEN)]
    (cond
      (= players g/MAX-PLAYERS) nil
      (= 1 players) reset
      :else added)))
(defmethod tick-len :chat [_] 1000)
(defmethod tick :chat [chat]
  (cond
    (zero? (:ticks-left chat))
      (reduce g/add-player (g/rand-game) (:players chat))
    (< 1 (count (:players chat)))
      (update chat :ticks-left dec)
    :else chat))
(defmethod on-action :chat [chat _ _] chat)
(defmethod leave-game :chat [chat pid]
  (update chat :players s/difference #{pid}))

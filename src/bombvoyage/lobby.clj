(ns bombvoyage.lobby
  (:require [clojure.core.async
             :refer [chan timeout go-loop go <! close! >!]
             :as a]
            [clojure.core.match :refer [match]]
            [bombvoyage.game :as g]
            [clojure.set :as s]))

(defn game-type [state & _] (:type state))

(defmulti join-game game-type)
(defmulti tick-len game-type)
(defmulti tick game-type)
(defmulti on-action game-type)
(defmulti leave-game game-type)

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
  (go
    (<! (timeout tick))
    (>! in [:tick])))

(defn game-loop [init-state in out sub]
  (go-loop [state init-state players #{}]
    (match (<! in)
      [:join pid pchan]
          (if-let [nex (join-game state pid)]
            (do (subscribe pid pchan sub)
                (>! out [:set-state nex])
                (recur nex (conj players pid)))
            (do (close! pchan)
                (recur state players)))
      [:close pid]
          (let [nex (leave-game state pid)
                nex-players (s/difference players #{pid})]
            (>! out [:set-state nex])
            (when-not (empty? nex-players)
              (recur nex nex-players)))
      [:tick]
          (let [nex (tick state)]
            (>! out [:set-state nex])
            (submit-tick in (tick-len nex))
            (recur nex players))
      [:message pid body]
          (let [nex (on-action state pid body)]
            (>! out [:set-state nex])
            (recur nex players)))))

(defn run-game-loop [init-state completion-fn]
  (let [in (chan)
        out (chan)
        sub [(a/mix in) (a/mult out)]]
    (go
      (<! (game-loop init-state in out sub))
      (close! in)
      (close! out)
      (completion-fn))
    (submit-tick in (tick-len init-state))
    in))

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
  (let [removed (update chat :players s/difference #{pid})
        reset-ticks (assoc removed :ticks-left 30)]
    (if (<= (count (:players chat)) 1)
      reset-ticks
      removed)))

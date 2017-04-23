(ns bombvoyage.game
  (:require [clojure.set :as s]))

;; Settings

(def WIDTH 13)
(def HEIGHT 13)
(def TILE-SIZE 48)
(def TICK-LEN 30)
(def WOODS 80)
(def POWER-UPS 10)
(def INIT-BOMBS 2)
(def INIT-LENGTH 2)
(def BOMB-TICKS 125)
(def EXPLOSION-TICKS 10)
(def SPEED 6)
(def MARGIN (* SPEED 2))
(def INIT-POS
  [[0 0] [(dec WIDTH) 0] [0 (dec HEIGHT)] [(dec WIDTH) (dec HEIGHT)]])
(def DELS
  {:none [0 0] :up [0 -1] :down [0 1] :right [1 0] :left [-1 0]})
(def MAX-PLAYERS (count INIT-POS))

;; Setup

(defn rand-subset [subset-size s]
  (->> (shuffle s)
       (take subset-size)
       set))

(defn rand-game
  "Makes a random board configuration."
  []
  (let [all-tiles
          (set (for [x (range WIDTH) y (range HEIGHT)] [x y]))
        stones
          (s/select (fn [[x y]] (and (odd? x) (odd? y))) all-tiles)
        no-wood
          (set (for [pos INIT-POS
                     diff (vals DELS)]
                 (map + diff pos)))
        woods
          (->> (s/union no-wood stones)
               (s/difference all-tiles)
               (rand-subset WOODS))
        bomb-ups
          (rand-subset POWER-UPS woods)
        length-ups
          (rand-subset POWER-UPS (s/difference woods bomb-ups))]
    {:tick 0
     :stones stones
     :woods woods
     :bomb-ups bomb-ups
     :length-ups length-ups
     :explosions []
     :bombs {}
     :players {}}))

(defn add-player
  "Adds a player to a game."
  [game id]
  (let [n (count (:players game))
        pos (nth INIT-POS n)
        player {:id id
                :pos (mapv * [TILE-SIZE TILE-SIZE] pos)
                :dir :right
                :moving false
                :bombs INIT-BOMBS
                :bomb-len INIT-LENGTH}]
    (assoc-in game [:players id] player)))

(defn remove-player
  "Removes a player from the game."
  [game id]
  (update game :players dissoc id))

;; Util

(defn board-coord [n]
  "Converts to a board coordinate"
  (-> n
      (+ (/ TILE-SIZE 2))
      (quot TILE-SIZE)))

(defn lock [n]
  "Locks a value to the pixel center if close enough, nil o/w"
  (let [diff (rem n TILE-SIZE)]
    (cond
      (<= 0 diff MARGIN) (- n diff)
      (>= diff (- TILE-SIZE MARGIN)) (+ n (- TILE-SIZE diff)))))

(defn locked? [n] (zero? (rem n TILE-SIZE)))

(defn shift [pos dir]
  "Shifts a pos in the specified direction."
  (->> (dir DELS)
       (map * [SPEED SPEED])
       (map + pos)))

(defn valid? [[x y :as pos] invalid]
  "Checks if a board coordinate is not occupied or OOB."
  (and
    (not (invalid pos))
    (<= 0 x (dec WIDTH))
    (<= 0 y (dec HEIGHT))))

;; Actions

(defn mk-bomb [pos len id]
  "Makes a bomb at the given position with the specified len."
  {:ticks-left BOMB-TICKS
   :id id
   :pos pos
   :len len})

(defn drop-bomb [game id]
  "Has the specified player drop a bomb if it is allowed to."
  (let [player (get-in game [:players id])
        board-coords (map board-coord (:pos player))
        new-bomb (mk-bomb board-coords
                          (:bomb-len player)
                          (:id player))]
    (if (or (zero? (:bombs player)) ((:bombs game) board-coords))
      game
      (-> game
          (update :bombs assoc board-coords new-bomb)
          (update-in [:players id :bombs] dec)))))

(defn start-moving [game id dir]
  "Sets the specified player to start moving in a specified dir."
  (-> game
      (assoc-in [:players id :moving] true)
      (assoc-in [:players id :dir] dir)))

(defn stop-moving [game id]
  "Tells the specified player to stop moving in the direction it is"
  (assoc-in game [:players id :moving] false))

;; Tick

(defn move-player [{:keys [pos dir moving] :as player} invalid]
  "Moves a player, takes into account possible manuevering to position
  so it can make turns. Blocks up against obstacles or borders."
  (let [[x y] pos
        [locked-x locked-y] (map locked? pos)
        board-pos (map board-coord pos)
        nex-tile (map + board-pos (dir DELS))]
    (cond
      (not moving) player
      (and locked-x locked-y (not (valid? nex-tile invalid))) player
      (and (#{:left :right} dir) locked-y)
        (assoc player :pos (shift pos dir))
      (and (#{:up :down} dir) locked-x)
        (assoc player :pos (shift pos dir))
      (and (lock x) (lock y))
        (assoc player :pos [(lock x) (lock y)])
      :else player)))

(defn move-player-transform [game]
  "Applies the move-player operation to each player in a game."
  (let [{:keys [stones woods bombs players]} game
        invalid (set (concat stones woods (keys bombs)))]
    (->> (vals (:players game))
         (map #(move-player % invalid))
         (map (juxt :id identity))
         (into {})
         (assoc game :players))))

(defn tick-down-bomb-transform [game]
  "Ticks down each bomb in the game"
  (let [bomb-pos (keys (:bombs game))
        tick-down #(update-in %1 [:bombs %2 :ticks-left] dec)]
    (reduce tick-down game bomb-pos)))

(defn tick-down-explosion-transform [game]
  "Ticks down each explosion in the game"
  (let [explosions (:explosions game)
        updated (map #(update % :ticks-left dec) explosions)
        cleaned (remove #(zero? (:ticks-left %)) updated)]
    (assoc game :explosions cleaned)))

(defn add-explosion [game pos dir]
  "Adds a explosion at the specified position and direction in game"
  (let [explosion {:pos pos :dir dir :ticks-left EXPLOSION-TICKS}]
      (update game :add conj explosion)))

(declare explode-bomb)
(defn walk-explosion [game [x y :as pos] dir len]
  "Walks the explosion in a specified direction."
  (cond
    (zero? len) game
    (not (<= 0 x (dec WIDTH))) game
    (not (<= 0 y (dec HEIGHT))) game
    ((:stones game) pos) game
    ((:woods game) pos)
      (let [to-remove (conj (:remove game) pos)]
        (-> game
            (add-explosion pos dir)
            (assoc :remove to-remove)))
    ((:bombs game) pos)
      (-> game
          (add-explosion pos dir)
          (explode-bomb ((:bombs game) pos))
          (recur (map + (dir DELS) pos) dir (dec len)))
    :else
      (-> game
          (add-explosion pos dir)
          (recur (map + (dir DELS) pos) dir (dec len)))))

(defn explode-bomb [game bomb]
  "Explodes a specific bomb and gives it back to the player."
  (-> game
      (update-in [:players (:id bomb) :bombs] inc)
      (update :bombs dissoc (:pos bomb))
      (walk-explosion (:pos bomb) :up (:len bomb))
      (walk-explosion (:pos bomb) :down (:len bomb))
      (walk-explosion (:pos bomb) :right (:len bomb))
      (walk-explosion (:pos bomb) :left (:len bomb))))

(defn explode-bomb-transform [game]
  "Explodes every bomb in the game"
  (let [bombs (vals (:bombs game))
        explode (filter #(zero? (:ticks-left %)) bombs)]
    (as-> game game
      (assoc game :remove #{})
      (assoc game :add #{})
      (reduce explode-bomb game explode)
      (let [new-woods (s/difference (:woods game) (:remove game))]
        (assoc game :woods new-woods))
      (update game :explosions concat (:add game))
      (dissoc game :remove)
      (dissoc game :add))))

(defn do-pick-up [game id player]
  "The player picks up a powerup if it exists at that position."
  (let [pos (map board-coord (:pos player))]
    (cond
      ((:length-ups game) pos)
        (-> game
            (assoc-in [:players id] (update player :bomb-len inc))
            (update :length-ups s/difference #{pos}))
      ((:bomb-ups game) pos)
        (-> game
            (assoc-in [:players id] (update player :bombs inc))
            (update :bomb-ups s/difference #{pos}))
      :else game)))

(defn pick-up-transform [game]
  "Does the pick-up op for each player in the game"
  (reduce-kv do-pick-up game (:players game)))

(defn tick [game]
  "Does each transformation for one tick in the game"
  (-> game
      move-player-transform
      tick-down-bomb-transform
      tick-down-explosion-transform
      explode-bomb-transform
      pick-up-transform))

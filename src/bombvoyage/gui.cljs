(ns bombvoyage.gui
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [bombvoyage.game :as g]
            [clojure.set :as s]
            [cljs.reader :refer [read-string]]
            [goog.dom :as dom]
            [goog.events :as events]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [<! >! put! chan timeout]]
            [reagent.core :as r]))

(enable-console-print!)

(defn log [& args]
  (println (apply str args))
  (first args))

(def game-state (r/atom nil))

(defn set-status! [& args]
  (reset! game-state
          {:type :status-msg
           :msg (apply str args)}))

(set-status! "Setting up page!")

(defn render-wood [[x y]] ^{:key (str [x y])}
  [:rect {:x (* g/TILE-SIZE x)
          :y (* g/TILE-SIZE y)
          :width g/TILE-SIZE
          :height g/TILE-SIZE
          :fill "#ffff9e"}])

(defn render-stone [[x y]] ^{:key (str [x y])}
  [:rect {:x (* g/TILE-SIZE x)
          :y (* g/TILE-SIZE y)
          :width g/TILE-SIZE
          :height g/TILE-SIZE
          :fill "#c4c4c4"}])

(defn render-player [{[x y] :pos id :id}] ^{:key (str [id x y])}
  [:image {:x x
           :y y
           :width g/TILE-SIZE
           :height g/TILE-SIZE
           :xlinkHref "/monkey_sprite.png"}])

(defn render-length-up [[x y]] ^{:key (str [x y])}
  [:image {:x (* g/TILE-SIZE x)
           :y (* g/TILE-SIZE y)
           :width g/TILE-SIZE
           :height g/TILE-SIZE
           :xlinkHref "/star.png"}])

(defn render-bomb-up [[x y]] ^{:key (str [x y])}
  [:image {:x (* g/TILE-SIZE x)
           :y (* g/TILE-SIZE y)
           :width g/TILE-SIZE
           :height g/TILE-SIZE
           :xlinkHref "/star2.png"}])

(defn center [x] (-> x (* g/TILE-SIZE) (+ (/ g/TILE-SIZE 2))))

(defn render-bomb [{:keys [pos ticks-left]}]
  (let [[x y] (map center pos)
        col (+ 40 (* 2 ticks-left))] ^{:key (str pos)}
    [:circle {:cx x
              :cy y
              :fill (str "rgb(255, " col ", " col ")")
              :r 16}]))

(defn render-explosion [{:keys [pos ticks-left dir]}]
  (let [[x y] pos] ^{:key (str pos dir)}
    (if (#{:left :right} dir)
      [:rect {:x (* g/TILE-SIZE x)
              :y (+ 10 (* g/TILE-SIZE y))
              :width g/TILE-SIZE
              :height (- g/TILE-SIZE (* 2 10))
              :fill "rgb(255, 40, 40)"}]
      [:rect {:x (+ 10 (* g/TILE-SIZE x))
              :y (* g/TILE-SIZE y)
              :width (- g/TILE-SIZE (* 2 10))
              :height g/TILE-SIZE
              :fill "rgb(255, 40, 40)"}])))

(defmulti render :type)
(defmethod render :game [game-state]
  [:svg
   {:width (* g/TILE-SIZE g/WIDTH)
    :height (* g/TILE-SIZE g/HEIGHT)}
   [:rect {:width (* g/TILE-SIZE g/WIDTH)
           :height (* g/TILE-SIZE g/HEIGHT)
           :fill "#8cff8c"}]
   [:g (map render-length-up (:length-ups game-state))]
   [:g (map render-bomb-up (:bomb-ups game-state))]
   [:g (map render-stone (:stones game-state))]
   [:g (map render-wood (:woods game-state))]
   [:g (map render-bomb (vals (:bombs game-state)))]
   [:g (map render-explosion (:explosions game-state))]
   [:g (map render-player
            (filter :alive? (vals (:players game-state))))]])

(defmethod render :chat [chat]
  (if (<= (:players chat) 1)
    [:div.text-center [:h1 "Waiting for more players!"]]
    [:div.text-center
     [:h1 "Starting in " (:ticks-left chat) " seconds."]
     [:h1 "Players in game: " (:players chat)]]))

(defmethod render :status-msg [status]
  [:div.text-center [:h1 (:msg status)]])

(defmethod render :default [state]
  [:div.text-center [:h1 "State: " (str state)]])

(defn render-state [state]
  [render @state])

(r/render [render-state game-state] (js/document.getElementById "app"))

(defn get-key [e]
  "returns the key that was pressed from an event"
  (case (do (.preventDefault e) (.-keyCode e))
    37 :left
    38 :up
    39 :right
    40 :down
    32 :space
    nil))

(def keys-chan (chan))
(def events-chan (chan))

(defn handle-key-press [e action]
  "dispatches a key press to the event loop"
  (let [dir (get-key e)]
    (if dir
      (put! keys-chan [action dir]))))

(events/listen js/window (.-KEYDOWN events/EventType)
               #(handle-key-press % :down))
(events/listen js/window (.-KEYUP events/EventType)
               #(handle-key-press % :up))

(defn process-keys []
  "starts an event loop that process all the presses"
  (go-loop [pressed nil]
    (let [[action dir] (<! keys-chan)]
      (cond
        (= [:down :space] [action dir])
            (do
              (>! events-chan [:drop-bomb])
              (recur pressed))
        (and (not= pressed dir) (= action :down))
            (do
              (>! events-chan [:start-moving dir])
              (recur dir))
        (and (= pressed dir) (= action :up))
            (do
              (>! events-chan [:stop-moving])
              (recur nil))
        :else (recur pressed)))))

(def game-token
  (-> js/document
      (.getElementById "data")
      (.getAttribute "data-game-info")))

(def game-server
  (-> js/document
      (.getElementById "data")
      (.getAttribute "data-game-server")))

(defn sock-conn []
  (go
    (set-status! "Connecting...")
    (when-let [ws-ch (:ws-channel (log (<! (ws-ch game-server))))]
      (set-status! "Sending the game-token")
      (>! ws-ch game-token)
      (set-status! "Getting my player id")
      (let [[_ me] (:message (<! ws-ch))]
        (set-status! "Got id: " me ". Syncing state to server...")
        (loop []
          (let [[v p] (alts! [ws-ch events-chan])]
            (when-not (and (nil? v) (= ws-ch p))
              (println v)
              (if (= ws-ch p)
                (reset! game-state (second (:message v)))
                (>! ws-ch v))
              (recur))))
        (set-status! "Disconnected! Maybe the game is full?")))))

(defn run []
  (do
    (process-keys)
    (sock-conn)))

(run)

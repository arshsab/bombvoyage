(ns bombvoyage.fileserv
  (:require [org.httpkit.server :refer [run-server]]
            [compojure.core :refer :all]
            [compojure.handler :refer [site]]
            [clojure.core.async :refer [thread <!! timeout]]
            [clj-http.client :as client]
            [clj-time.core :as t]
            [clj-jwt.core :refer :all]
            [compojure.route :as route]
            [hiccup.core :refer [html]]
            [bombvoyage.game :as g]))

(def game-ticker (atom 0))
(def games (atom {}))
(def ^:dynamic *config* nil)

(defn start-game-update-loop []
  (thread
    (loop []
      (->> *config*
           :game-servers
           (map #(str "http://" %))
           (map client/get)
           (map :body)
           (map read-string)
           (reduce merge)
           (reset! games))
      (println @games)
      (<!! (timeout 2000))
      (recur))))

(defn wrap-body [body]
  (html
    [:html
     [:head
      [:link {:rel :stylesheet
              :type "text/css"
              :href "https://maxcdn.bootstrapcdn.com/bootswatch/3.3.7/cyborg/bootstrap.min.css"}]]
     [:body {:font-size "250%"}
       body]]))

(defn index []
  (wrap-body
    [:div.container
      [:div.page-header [:h1 "BombVoyage"]]
      [:p.text-center [:a.btn.btn-success {:type :button :href "/create"} "Create"]]
      (for [[game-id _] @games]
        [:p.text-center [:a.btn.btn-info
          {:href (str "/game/" game-id)}
          "Join Game #" game-id]])]))

(defn game-screen [token server]
  (let [payload
          {:game-token token
           :exp (t/plus (t/now) (t/seconds 5))}
        encoded (-> payload jwt (sign (:secret *config*)) to-str)]
    (wrap-body
       [:div.text-center
        [:span#data {:data-game-server server
                     :data-game-info encoded}]
        [:div#app]
        [:script {:src "/main.js"}]])))

(defn create []
  (let [game-id (swap! game-ticker inc)
        server (rand-nth (:game-servers *config*))]
    (game-screen [:create game-id] (str "ws://" server "/ws"))))

(defn join [game-id]
  (let [server (@games game-id)]
    (game-screen [:join game-id] (str "ws://" server "/ws"))))

(defroutes app-routes
  (GET "/" [] (index))
  (GET "/game/:id" [id] (join (read-string id)))
  (GET "/create" [] (create))
  (route/resources "/")
  (route/not-found "<h1>Page not found</h1>"))

(def handler (site #'app-routes))

(defn run [config]
  (do
    (alter-var-root
      (var *config*)
      (fn [_] config))
    (start-game-update-loop)
    (run-server
      handler
      {:port (:port config)})))

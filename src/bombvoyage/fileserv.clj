(ns bombvoyage.fileserv
  (:require [org.httpkit.server :refer [run-server]]
            [compojure.core :refer :all]
            [compojure.handler :refer [site]]
            [clojure.core.async :refer [thread <!! timeout]]
            [clj-http.client :as client]
            [compojure.route :as route]
            [hiccup.core :refer [html]]
            [bombvoyage.game :as g]))

(def game-ticker (atom 0))
(def games (atom {}))
(def game-servers ["localhost:8081"])

(defn start-game-update-loop []
  (thread
    (loop []
      (->> game-servers
           (map #(str "http://" %))
           (map client/get)
           (map :body)
           (map read-string)
           (reduce merge)
           (reset! games))
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
      [:div.container
       [:div.page-header [:h1 "BombVoyage"]]
       body]]]))

(defn index []
  (wrap-body
    [:div
      [:p.text-center [:a.btn.btn-success {:type :button :href "/create"} "Create"]]
      (for [[game-id _] @games]
        [:p.text-center [:a.btn.btn-info
          {:href (str "/game/" game-id)}
          "Join Game #" game-id]])]))

(defn game-screen [token]
  (wrap-body
     [:div.text-center
      [:span#data {:data-game-info (prn-str token)}]
      [:div#app]
      [:script {:src "/main.js"}]]))

(defn create []
  (let [game-id (swap! game-ticker inc)
        server (rand-nth game-servers)]
    (game-screen [:create (str "ws://" server "/ws") game-id])))

(defn join [game-id]
  (let [server (@games game-id)]
    (game-screen [:join (str "ws://" server "/ws") game-id])))

(defroutes app-routes
  (GET "/" [] (index))
  (GET "/game/:id" [id] (join (read-string id)))
  (GET "/create" [] (create))
  (route/resources "/")
  (route/not-found "<h1>Page not found</h1>"))

(def handler (site #'app-routes))

(defn run []
  (do
    (start-game-update-loop)
    (run-server
      handler
      {:port 8080})))

(ns bombvoyage.fileserv
  (:require [org.httpkit.server :refer [run-server]]
            [compojure.core :refer :all]
            [compojure.handler :refer [site]]
            [clojure.core.async :refer [thread <!! timeout]]
            [clj-http.client :as client]
            [compojure.route :as route]
            [hiccup.core :refer [html]]
            [bombvoyage.game :as g])
  (:gen-class))

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

(defn index []
  (html
    [:html
     [:head]
     [:body {:font-size "250%"}
      [:p [:a {:href "/create"} "Create"]]
      (for [[game-id _] @games]
        [:p [:a
          {:href (str "/game/" game-id)}
          "Join Game #" game-id]])]]))

(defn create []
  (let [game-id (swap! game-ticker inc)
        server (rand-nth game-servers)]
    (html
      [:html
       [:head]
       [:body {:style "background-color: 050002"}
        [:span#data {:data-game-info
                      (prn-str
                        [:create
                         (str "ws://" server "/ws")
                         game-id])}]
        [:div#app {:style "width: 50%; margin: 0 auto"}]
        [:script {:src "main.js"}]]])))

(defn game [game-id]
  (html
    [:html
     [:head]
     [:body {:style "background-color: 050002"}
      [:span#data {:data-game-info
                   (let [server (@games game-id)]
                      (prn-str
                        [:join
                         (str "ws://" server "/ws")
                         game-id]))}]
      [:div#app {:style "width: 50%; margin: 0 auto"}]
      [:script {:src "/main.js"}]]]))

(defroutes app-routes
  (GET "/" [] (index))
  (GET "/game/:id" [id] (game (read-string id)))
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

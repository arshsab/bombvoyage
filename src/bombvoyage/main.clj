(ns bombvoyage.main
  (:require [bombvoyage.fileserv :as f]
            [bombvoyage.sockserv :as s])
  (:gen-class))

(defn -main [& args]
  (case (first args)
    "files" (f/run)
    "game" (s/run)
    (println "Must run with mode argument.
             One of \"files\" or \"game\".")))

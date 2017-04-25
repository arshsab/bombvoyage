(ns bombvoyage.main
  (:require [bombvoyage.fileserv :as f]
            [immuconf.config :as conf]
            [bombvoyage.sockserv :as s])
  (:gen-class))

(defn -main [& args]
  (if-not (first args)
    (println "Must specify a config file")
    (let [config (conf/load (first args))]
      (case (:serv-type config)
        :fileserv (f/run config)
        :gameserv (s/run config)
        (println ":serv-type must be #{:fileserv :gameserv}")))))

(defproject bombvoyage "0.1.0-SNAPSHOT"
  :plugins [[lein-cljsbuild "1.1.5"]]
  :cljsbuild {
      :builds [{
          :source-paths ["src/bombvoyage"]
          :compiler {
              :output-to "resources/public/main.js"
              :output-dir "resources/public"
              :optimizations :advanced
              :source-map "resources/public/main.js.map"
              :pretty-print true}}]}
  :main bombvoyage.main
  :dependencies [[javax.servlet/servlet-api "2.5"]
                 [org.clojure/clojure "1.8.0"]
                 [clj-jwt "0.1.1"]
                 [org.clojure/core.async "0.3.442"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [levand/immuconf "0.1.0"]
                 [org.clojure/clojurescript "1.9.518"]
                 [jarohen/chord "0.8.1"]
                 [clj-http "2.3.0"]
                 [ring/ring-json "0.4.0"]
                 [hiccup "1.0.5"]
                 [reagent "0.6.1"]
                 [compojure "1.5.2"]
                 [http-kit "2.2.0"]])

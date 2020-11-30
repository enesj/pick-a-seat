(ns pickaseat.ver01.user
  (:require
    [mount.core :as mount :refer [defstate]]
    [compojure.core :refer [routes defroutes GET POST]]
    [compojure.handler :as handler]
    [compojure.route :as route]
    [clojure.tools.namespace.repl :as tn]
    [ring.util.response :as resp]
    [ring.adapter.jetty :refer [run-jetty]]))


(defroutes app
           (GET "/" []
                (resp/resource-response "index.html" {:root "public"}))
           (route/resources "/"))



(defn www-server [port]
  (-> (routes app)
      (handler/site)
      (run-jetty {:join? false
                  :port port})))

(defstate www
  :start (www-server 3000)
  :stop (.stop www))

(defn start []
  (mount/start #'www))

(defn stop []
  (mount/stop))


(defn refresh []
  (stop)
  (tn/refresh))

(defn refresh-all []
  (stop)
  (tn/refresh-all))

(defn go
  "starts all states defined by defstate"
  []
  (start)
  :ready)

(defn reset
  "stops all states defined by defstate, reloads modified source files, and restarts the states"
  []
  (stop)
  (tn/refresh :after 'pickaseat.ver01.user/go))

(mount/in-clj-mode)


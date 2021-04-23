(ns charybdis.core
  (:require [org.httpkit.server :as server]
            [clojure.string :as str]))

(defonce server (atom nil))


(defn handler [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str (:uri req))})

(def routes
  {:.  {:get handler}
   "a" {:.    {:get handler}
        [:id] {:.  {:get handler}
               "b" {:get handler}}}})

(defn make-handler [routes {:keys [uri request-method] :as req}]
  (let [route (-> uri
                  (str/split #"/")
                  (as-> $ (remove str/blank? $))
                  vec
                  (into [:. request-method]))]
    (reduce (fn [{:keys [handler] :as acc} v]
              (if-let [next (get handler v)]
                (assoc acc :handler next)
                (if-let [p  (first (filter vector? (keys handler)))]
                  (-> acc
                      (assoc-in [:request :route-params (first p)] v)
                      (assoc-in [:handler] (get handler p)))
                  (assoc acc :handler nil))))
            {:handler routes :request req}
            route)))

(make-handler routes {:request-method :post :uri "/"})

(defn app [req]
  (let [{:keys [handler request]} (make-handler routes req)]
    (if handler
      (handler request)
      {:status 404
       :headers {"Content-Type" "text/html"}
       :body "404 Route not found"})))

(defn start-server []
  (when-not @server
    (reset! server (server/run-server app {:port 3333}))
    [:ok]))

(defn stop-server []
  (when @server
    (@server)
    (reset! server nil)
    [:ok]))

(comment
  (start-server)
  (stop-server))

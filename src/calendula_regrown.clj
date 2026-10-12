(ns calendula-regrown
  (:require
   [org.httpkit.server :as httpkit]
   [hiccup.page]))

(defonce !system (atom {:server nil :connections {}}))

;; app

;; 1 - identify the right user.
;; serve html -> who is this?

(defn index [_]
  [:strong "guten morgen."])

(def routes
  {[:get "/"] #'index})

(defn req->state [req])

;; lifecycle

(def head
  [:head
   [:meta {:charset "UTF-8"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]])

(defn app [{:keys [request-method uri] :as req}]
  (cond
    (= :head request-method)
    {:status 202}

    (contains? routes [request-method uri])
    {:status 200
     :headers {"content-type" "text/html"}
     :body (hiccup.page/html5 head
                              ((get routes [request-method uri])
                               (req->state req)))}))

(defn halt! []
  (swap! !system update :server #(when % (httpkit/server-stop! %))))

(defn serve! [opts]
  (halt!)
  (swap! !system assoc :server
         (httpkit/run-server #'app (assoc opts :legacy-return-value? false))))

(comment
  !system
  )

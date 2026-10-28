(ns render-one
  "Just render from the JVM; no client actions hit the server"
  (:require [clojure.string :as str]
            [torque]
            [org.httpkit.server :as httpkit]
            [hiccup2.core :as h]))

;; ── system ────────────────────────────────────────────────────────────────

(def dev?
  (nil? (System/getenv "GARDEN_TOKEN")))

(defonce !system
  (atom {:datastar/connections {}}))

;; ── render ────────────────────────────────────────────────────────────────

(defn index [_]
  [:main#main "hello, there!"])

(def routes
  {[:get "/"] #'index})

;; ── fat morph ─────────────────────────────────────────────────────────────

(defmethod print-method org.httpkit.server.AsyncChannel [ch ^java.io.Writer w]
  (.write w "#httpkit/channel ")
  (print-method (str ch) w))

(defn sse-patch-elements [html]
  (str "event: datastar-patch-elements\n"
       (apply str (map #(str "data: elements " % "\n") (str/split-lines html)))
       "\n"))

(defn re-render! []
  (doseq [[ch conn-state] (:datastar/connections @!system)]
    (let [encode (:connection/encoder conn-state)]
      (httpkit/send! ch
                     (encode (sse-patch-elements (str (h/html {:mode :html} (index {})))))
                     false))))

(defn stream [req]
  (httpkit/as-channel
   req
   {:on-open
    (fn [ch]
      (let [encoder+headers (torque/pick-encoder req)]
        (httpkit/send! ch {:status 200
                           :headers (into {"content-type" "text/event-stream"
                                           "cache-control" "no-cache"}
                                          (:headers encoder+headers))}
                       false)
        (swap! !system assoc-in [:datastar/connections ch :connection/encoder] (:encode encoder+headers))))

    :on-close
    (fn [ch _status]
      (swap! !system update :datastar/connections dissoc ch))}))

(comment
  (:datastar/connections @!system)
  )

;; ── machinery ─────────────────────────────────────────────────────────────

(def head
  [:head
   [:meta {:charset "UTF-8"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
   [:script {:type "module" :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.4/bundles/datastar.js"}]])

(defonce !last-req (atom nil))
(comment @!last-req)

(defn app [{:keys [request-method uri] :as req}]
  (reset! !last-req req)
  (cond
    (= :head request-method)
    {:status 202}

    (and (= :get request-method) (= "/stream" uri))
    (stream req)

    (and (= :get request-method)
         (contains? routes [request-method uri]))
    {:status 200
     :headers {"content-type" "text/html"}
     :body (str (h/html {:mode :html}
                  (h/raw "<!DOCTYPE html>\n")
                  [:html
                   head
                   [:body {:data-init "@get('/stream')"}
                    ((get routes [request-method uri])
                     {})]]))}))

;; ── lifecycle ─────────────────────────────────────────────────────────────

(comment
  (torque/serve! #'app {})
  )

;; ── dev ───────────────────────────────────────────────────────────────────

;; Refresh connected clients on buffer eval
(when dev? (re-render!))

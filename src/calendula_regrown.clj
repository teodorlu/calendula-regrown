(ns calendula-regrown
  (:require
   [org.httpkit.server :as httpkit]
   [hiccup.page]
   [nextjournal.garden-id :as garden-id]
   [ring.middleware.session]
   [clojure.set :as set]))

(defonce !system
  (atom {:system/httpkit-server nil
         :system/datomic-conn nil
         :system/datastar-conns {}}))

;; ── app ───────────────────────────────────────────────────────────────────

(defn index [state]
  ;; (prn state)
  (let [uname (get-in state [:system/user :user/username])]
    (list [:strong "guten morgen."]
          [:p (if uname
                [:span (str uname " ") [:a {:href garden-id/logout-uri} "(logout)"]]
                [:a {:href garden-id/login-uri} "login"])])))

(def routes
  {[:get "/"] #'index})

;; ── machinery ─────────────────────────────────────────────────────────────

(defn req->state [req]
  (merge {}
         (when-let [user (get-in req [:session :user])]
           {:system/user
            (set/rename-keys user
                             {:uuid :user/uuid
                              :email :user/email
                              :name :user/name
                              :username :user/username})})))

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

(def wrapped-app
  (-> #'app
      garden-id/wrap-auth
      ring.middleware.session/wrap-session))

;; ── lifecycle ─────────────────────────────────────────────────────────────

(defn halt! []
  (swap! !system update :system/httpkit-server #(when % (httpkit/server-stop! %))))

(defn serve! [opts]
  (halt!)
  (swap! !system assoc :system/httpkit-server
         (httpkit/run-server #'wrapped-app (assoc opts :legacy-return-value? false))))

(comment
  !system
  )

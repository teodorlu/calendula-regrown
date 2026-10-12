(ns calendula-regrown
  (:require
   [clojure.string :as str]
   [org.httpkit.server :as httpkit]
   [hiccup.page]
   [nextjournal.garden-id :as garden-id]
   [ring.middleware.session]
   [ring.util.codec]
   [clojure.set :as set]
   [datomic.client.api :as d])
  (:import (java.time LocalDate DayOfWeek)
           (java.time.temporal TemporalAdjusters)))

(defonce !system
  (atom {:httpkit/server nil
         :datomic/conn nil
         :datastar/connections {}}))

(def availability {:monday [{:start 9 :end 10}
                            {:start 12 :end 15}]
                   :wednesday [{:start 8 :end 10}
                               {:start 12 :end 14}]})

;; ── datomic ───────────────────────────────────────────────────────────────

(def datomic-client
  (d/client {:server-type :datomic-local
             :storage-dir (do (assert (System/getenv "GARDEN_STORAGE"))
                              (str (System/getenv "GARDEN_STORAGE") "/datomic"))
             :system "garden"}))

(def schema
  [{:db/ident :user/uuid
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}

   ])
#_(swap! !system assoc :datomic/conn (connect-datomic))

(defn connect-datomic []
  (d/create-database datomic-client {:db-name "calendula-regrown"})
  (doto (d/connect datomic-client {:db-name "calendula-regrown"})
    (d/transact {:tx-data schema})))

;; ── date stuff ────────────────────────────────────────────────────────────

(comment
  (set! *warn-on-reflection* true))

(defn now [] (str (LocalDate/now)))

(defn before? [a b]
  (.isBefore (LocalDate/parse a) (LocalDate/parse b)))

(defn start-of-week [date]
  (str (LocalDate/.with (LocalDate/.plusDays (LocalDate/parse date) 1)
                        (TemporalAdjusters/previous DayOfWeek/MONDAY))))

(defn offset-week [date offset]
  (str (LocalDate/.plusWeeks (LocalDate/parse date) offset)))

(defn dates-of-week [start-date]
  (map (fn [offset] (str (LocalDate/.plusDays (LocalDate/parse start-date) offset))) (range 7)))

(defn date->keyword [day]
  (keyword (str/lower-case (str (LocalDate/.getDayOfWeek (LocalDate/parse day))))))

(defn free? [availability day hour]
  (and (some (fn [{:keys [start end]}] (<= start hour end)) (availability (date->keyword day)))
                                        ;(not (get-in @impulse/state [:appointments day hour]))
       ))

(comment
  (before? "2024-09-16" "2024-09-17") ;=> true
  (before? "2024-09-17" "2024-09-16") ;=> false
  (dates-of-week (start-of-week (now)))
  (start-of-week (now))
  (offset-week (start-of-week (now)) 1)
  (dates-of-week (offset-week (start-of-week (now)) 1))
  (date->keyword "2024-01-01") ;=> :monday
  (free? {:monday [{:start 12 :end 15}]} "2024-01-01" 15)
  ,)

;; ── app ───────────────────────────────────────────────────────────────────

(defn render-day [state day]
  [:div {:id (str "date-" day)}
   [:strong (str (name (date->keyword day)))]
   [:span " (" day ")"]
   (for [hour (mapcat #(range (:start %) (:end %)) (availability (date->keyword day)))
         :let [status (cond (free? availability day hour) :free
                                        ; (= (get-in @impulse/state [:appointments day hour]) (email req)) :mine
                            :else nil)]
         :when status]     ; either free or booked by the current user
     [:div {:hx-post (str (if (= status :free)
                            "/book-appointment?"
                            "/unbook-appointment?")
                          (ring.util.codec/form-encode {:day day :hour hour}))
            :hx-target (str "#date-" day)}
      [:input {:type "checkbox"
               :checked (= status :mine)}]
      (str hour ":00")])])

(defn render-week [state start-of-week]
  [:div#week
   [:div.grid
    (for [date (dates-of-week start-of-week)
          :when (and (availability (date->keyword date))
                     (not (before? date (:world/today state))))]
      (render-day state date))]
   [:div {:role "group" :style "margin-top: 1em"}
    [:button.outline (if (before? start-of-week (:world/today state))
                       {:disabled true}
                       {})
     "← prev week"]
    [:button.outline {} "next week →"]]])

(defn index [state]
  (let [uname (get-in state [:session/login :user/username])]
    (list [:strong "guten morgen."]
          [:p (if uname
                [:span (str uname " ") [:a {:href garden-id/logout-uri} "(logout)"]]
                [:a {:href garden-id/login-uri} "login"])]
          (when uname
            (render-week state (start-of-week (:world/today state)))))))

(def routes
  {[:get "/"] #'index})

;; ── machinery ─────────────────────────────────────────────────────────────

(defonce !last-req (atom nil))

(defn req->state [req]
  (reset! !last-req req)
  (merge {:world/today (now)}
         (when-let [user (get-in req [:session :user])]
           {:session/login
            (set/rename-keys user
                             {:uuid :user/uuid
                              :email :user/email
                              :name :user/name
                              :username :user/username})})))

(comment
  (keys state)
  (def state (req->state @!last-req))
  (index state)
  )

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

(defonce wrapped-app ; defonce prevents eval-bffer from logging out everyone
  (-> #'app
      garden-id/wrap-auth
      ring.middleware.session/wrap-session))

;; ── lifecycle ─────────────────────────────────────────────────────────────

(defn halt! []
  (swap! !system update :httpkit/server #(when % (httpkit/server-stop! %))))

(defn serve! [opts]
  (halt!)
  (swap! !system assoc :httpkit/server
         (httpkit/run-server #'wrapped-app (assoc opts :legacy-return-value? false)))
  (swap! !system assoc :datomic/conn (connect-datomic)))

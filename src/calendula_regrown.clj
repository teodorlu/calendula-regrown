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

(def availability {:monday [{:start 9 :end 10}
                            {:start 12 :end 15}]
                   :wednesday [{:start 8 :end 10}
                               {:start 12 :end 14}]})

;; ── date stuff ────────────────────────────────────────────────────────────

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

;; ── system ────────────────────────────────────────────────────────────────

(defonce !system
  (atom {:httpkit/server nil
         :datomic/conn nil
         :datastar/connections {}}))

(defn system->state [system]
  (let [conn (:datomic/conn system)]
    (cond-> {:world/today (now)}
      conn (assoc :datomic/db (d/db conn)))))

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

   {:db/ident :user.view/weqek
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}

   ])
#_(swap! !system assoc :datomic/conn (connect-datomic))

(defn connect-datomic []
  (d/create-database datomic-client {:db-name "calendula-regrown"})
  (doto (d/connect datomic-client {:db-name "calendula-regrown"})
    (d/transact {:tx-data schema})))

;; ── app ───────────────────────────────────────────────────────────────────

(defn render-day [_state day]
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
                          (ring.util.codec/form-encode {:day day :hour hour}))}
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
    [:button.outline (if (or (before? start-of-week (:world/today state))
                             (= start-of-week (:world/today state)))
                       {:disabled true}
                       {:data-on:click "@post('/prev-week')"})
     "← prev week"]
    [:button.outline {:data-on:click "@post('/next-week')"} "next week →"]]])

(defn find-current-week [state user]
  (ffirst
   (d/q '[:find ?w
          :where
          [?u :user.view/week ?w]
          [?u :user/uuid ?uuid]
          :in $ ?uuid]
        (:datomic/db state)
        (:user/uuid user))))

(defn index [state]
  (let [user (:session/login state)
        username (:user/username user)]
    (list [:strong "guten morgen."]
          [:p (if username
                [:span (str username " ") [:a {:href garden-id/logout-uri} "(logout)"]]
                [:a {:href garden-id/login-uri} "login"])]
          (when username
            (render-week state (or (find-current-week state user)
                                   (start-of-week (:world/today state))))))))

(def routes
  {[:get "/"] #'index})

;; ── fat morph ─────────────────────────────────────────────────────────────

(defn re-render! [])

;; ── commands ──────────────────────────────────────────────────────────────
;;
;; POST /next-week
;; :commands/next-week
;;
;; POST /book-appointment?day=2026-10-14&hour=9
;; :commands/book-appointment "day" "2026-10-14" "hour" "9"
(comment
  (-> {"x" 1 "y" 2}
      ring.util.codec/form-encode
      ring.util.codec/form-decode)
  ,)

(defn update-week [offset state _params]
  (let [user (:session/login state)
        current (or (find-current-week state user)
                    (start-of-week (:world/today state)))]
    [[:datomic/transact [{:user/uuid (:user/uuid user)
                          :user.view/week (offset-week current offset)}]]]))

(comment
  (def state (create-state @!last-req))

  (find-current-week state (:session/login state))

  )

(def commands-by-uri
  {"/next-week" (partial #'update-week 1)
   "/prev-week" (partial #'update-week -1)})

(defn effectuate! [effects]
  (doseq [[kind args] effects]
    (case kind
      :datomic/transact
      (d/transact (:datomic/conn @!system) {:tx-data args})

      (throw (ex-info "Unsupported effect kind" {:kind kind})))))

(defn dispatch-command! [state req command-handler]
  (httpkit/as-channel req
                      {:on-open (fn [ch]
                                  (httpkit/send! ch {:status 202})
                                  (->> (command-handler state (:params req))
                                       effectuate!)
                                  (re-render!))}))

;; ── machinery ─────────────────────────────────────────────────────────────

(defn req->state [req]
  (when-let [user (get-in req [:session :user])]
    {:session/login
     (set/rename-keys user
                      {:uuid :user/uuid
                       :email :user/email
                       :name :user/name
                       :username :user/username})}))

(defn create-state [req]
  (merge (system->state @!system)
         (req->state req)))

(comment
  (keys state)
  (def state (create-state @!last-req))
  (index state)
  )

(def head
  [:head
   [:meta {:charset "UTF-8"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
   [:script {:type "module" :src "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.4/bundles/datastar.js"}]])

(defonce !last-req (atom nil))

(defn app [{:keys [request-method uri] :as req}]
  (reset! !last-req req)
  (cond
    (= :head request-method)
    {:status 202}

    (and (= :get request-method)
         (contains? routes [request-method uri]))
    {:status 200
     :headers {"content-type" "text/html"}
     :body (hiccup.page/html5 head
                              ((get routes [request-method uri])
                               (create-state req)))}

    (and (= :post request-method)
         (contains? commands-by-uri uri))
    (dispatch-command! (create-state req)
                       req
                       (commands-by-uri uri))))

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

(ns calendula-regrown
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [datomic.client.api :as d]
   [hiccup2.core :as h]
   [nextjournal.garden-id :as garden-id]
   [org.httpkit.server :as httpkit]
   [ring.middleware.session]
   [ring.util.codec]
   [torque])
  (:import
   (java.time DayOfWeek LocalDate)
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

(defn print-exception [{:exception/keys [origin data message _object]}]
  (println "Excption at" origin ":" message "," data))

(defonce !system
  (atom {:httpkit/server nil
         :datomic/conn nil
         :datastar/connections {}
         :exception/report-fn #'print-exception}))

(comment
  (swap! !system dissoc :exception/reporter)
  (swap! !system assoc :exception/report-fn #'print-exception)
  ,)

(defn report-exception!
  ([origin e] (report-exception! origin e nil))
  ([origin e extra-data]
   (when-let [report-fn (:exception/report-fn @!system)]
     (let [message (ex-message e)
           data (ex-data e)]
       (report-fn {:exception/origin origin
                   :exception/message message
                   :exception/data (merge extra-data data)})))))

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
    :db/doc "User UUID taken from Garden ID"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}

   {:db/ident :user.view/week
    :db/doc "Which week the user is currently viewing"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}

   ;; Dates and hours
   {:db/ident :time/date
    :db/doc "A date, formatted YYYY-MM-DD"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}

   {:db/ident :time/hour
    :db/doc "A hour, measured in time since midnight"
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   ;; discard.

   {:db/ident :appointment/date
    :db/doc "Date, formatted YYYY-MM-DD"
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}

   {:db/ident :appointment/hour
    :db/doc "A hour, measured in time since midnight"
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}

   {:db/ident :appointment/booked-by
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}])
#_(swap! !system assoc :datomic/conn (connect-datomic))

(defn connect-datomic []
  (d/create-database datomic-client {:db-name "calendula-regrown"})
  (doto (d/connect datomic-client {:db-name "calendula-regrown"})
    (d/transact {:tx-data schema})))

;; ── app ───────────────────────────────────────────────────────────────────

(defn create-book-expr [path date hour]
  (str "@post('" path "?" (ring.util.codec/form-encode {"date" date "hour" hour}) "')"))
#_(create-book-expr "/book-appointment" "2022-02-02" 10)

(defn render-day [_state day]
  [:div {:id (str "date-" day)}
   [:strong (str (name (date->keyword day)))]
   [:span " (" day ")"]
   (for [hour (mapcat #(range (:start %) (:end %)) (availability (date->keyword day)))
         :let [status (cond (free? availability day hour) :free

                            #_#_
                            (= (get-in @impulse/state [:appointments day hour]) (email req))
                            :mine

                            :else nil)]
         :when status]     ; either free or booked by the current user
     [:div
      [:input {:type "checkbox"
               :checked (= status :mine)
               :data-on:change
               (create-book-expr
                (if (= status :free) "/book-appointment" "/unbook-appointment")
                day hour)}]
      (str hour ":00")
      [:small " (" status " "
       [:button {:data-on:click (create-book-expr "/book-appointment" day hour)} "bk"]
       " "
       [:button {:data-on:click (create-book-expr "/unbook-appointment" day hour)} "ubk"]
       ")"]])])

(comment
  (def state (create-state @!last-req))

  )

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
    [:main#app
     [:strong "guten morgen."]
     [:p (if username
           [:span (str username " ") [:a {:href garden-id/logout-uri} "(logout)"]]
           [:a {:href garden-id/login-uri} "login"])]
     (when username
       (render-week state (or (find-current-week state user)
                              (start-of-week (:world/today state)))))
     [:p "Source: " [:a {:href "https://github.com/teodorlu/calendula-regrown"}
                     "github.com/teodorlu/calendula-regrown"]]]))

(def routes
  {[:get "/"] #'index})

;; ── fat morph ─────────────────────────────────────────────────────────────

(declare req->state)

(defmethod print-method org.httpkit.server.AsyncChannel [ch ^java.io.Writer w]
  (.write w "#httpkit/channel ")
  (print-method (str ch) w))

(defn sse-patch-elements [html]
  (str "event: datastar-patch-elements\n"
       (apply str (map #(str "data: elements " % "\n") (str/split-lines html)))
       "\n"))

(defn re-render! []
  (doseq [[ch conn-state] (:datastar/connections @!system)]
    (let [state (merge (system->state @!system) conn-state)
          encode (:connection/encoder conn-state)]
      (httpkit/send! ch
                     (encode (sse-patch-elements (str (h/html {:mode :html} (index state)))))
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
        (swap! !system assoc-in [:datastar/connections ch] (req->state req))
        (swap! !system assoc-in [:datastar/connections ch :connection/encoder] (:encode encoder+headers))))

    :on-close
    (fn [ch _status]
      (swap! !system update :datastar/connections dissoc ch))}))

(comment
  (:datastar/connections @!system)
  )

;; ── commands ──────────────────────────────────────────────────────────────

(defn update-week [offset state _params]
  (let [user (:session/login state)
        current (or (find-current-week state user)
                    (start-of-week (:world/today state)))]
    [[:datomic/transact [{:user/uuid (:user/uuid user)
                          :user.view/week (offset-week current offset)}]]]))

(defn book-appointment
  {:command/id :commands/book-appointment
   :command/uri "/book-appointment"}
  [state {:as params :strs [date hour]}]
  [[:datomic/transact [{:appointment/booked-by (select-keys (:session/login state) [:user/uuid])
                        :appointment/date date
                        :appointment/hour (parse-long hour)}]]])

(defn unbook-appointment
  {:command/id :commands/unbook-appointment
   :command/uri "/unbook-appointment"}
  [state params]
  (prn "TODO unbook appointment"))

(comment
  (def state (create-state @!last-req))
  (keys state)
  (d/q '[:find (pull ?a [*])
         :where [?a :appointment/booked-by]]
       (:datomic/db state))

  )

(defn next-week
  {:command/id :commands/next-week
   :command/uri "/next-week"}
  [state params]
  (update-week 1 state params))

(defn prev-week
  {:command/id :commands/prev-week
   :command/uri "/prev-week"}
  [state params]
  (update-week -1 state params))

(def command-vars
  #{#'next-week
    #'prev-week
    #'book-appointment
    #'unbook-appointment})

(defn by [index-fn items]
  (into {} (map (juxt index-fn identity)) items))

(def commands-by-uri (by (comp :command/uri meta) command-vars))

(def commands-by-id (by (comp :command/id meta) command-vars))

(defn transact [system args]
  (d/transact (:datomic/conn system) {:tx-data args}))

(def effect-handlers {:datomic/transact #'transact})

(defn effectuate! [effects]
  (doseq [[kind args] effects]
    (if-let [handler (get effect-handlers kind)]
      (try
        (handler @!system args)
        (catch Exception e
          (report-exception! :effectuation e)))
      (throw (ex-info "Unsupported effect kind" {:kind kind})))))

(defn dispatch-command! [state req command-handler]
  (httpkit/as-channel
   req
   {:on-open
    (fn [ch]
      (httpkit/send! ch {:status 202})
      (let [params (some-> req :query-string ring.util.codec/form-decode)]
        (try (effectuate! (command-handler state params))
             (catch Exception e
               (report-exception! :command-dispatch e {:handler command-handler
                                                       :params params})))))}))

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
                           (create-state req))]]))}

    (and (= :post request-method)
         (contains? commands-by-uri uri))
    (do (dispatch-command! (create-state req)
                           req
                           (commands-by-uri uri))
        (re-render!))))

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

;; ── dev ───────────────────────────────────────────────────────────────────

(def dev?
  (str/starts-with? (System/getenv "GARDEN_URL") "http://localhost"))

(when dev? (re-render!))

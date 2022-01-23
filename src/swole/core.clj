(ns swole.core
  (:require [clojure.string :refer [split trim]]
            [datahike.api :refer [q pull db transact] :as d]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.cookies :refer [wrap-cookies]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.util.response :refer [redirect]]
            [ring.util.codec :refer [url-encode url-decode]]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [hiccup.page :refer [html5 include-css]]
            [hiccup.form :refer [form-to text-field hidden-field submit-button select-options]]
            [hiccup.element :refer [link-to]]
            [java-time :refer [local-date zone-id time-between]]))


(defn schemon [ident type cardinality]
  {:db/ident ident
   :db/valueType type
   :db/cardinality cardinality})

(defn one [ident type]
  (schemon ident type :db.cardinality/one))

(defn many [ident type]
  (schemon ident type :db.cardinality/many))

(def schema [
             (assoc (one :name :db.type/string)
               :db/unique :db.unique/identity)
             (assoc (one :email :db.type/string)
               :db/unique :db.unique/value)
             (one :color :db.type/string)
             (one :yogi :db.type/ref)
             (one :reps :db.type/long)
             (one :figure :db.type/string)
             (one :offset :db.type/long)
             ])

(def cfg {:store {:backend :file :path "data"}})

(declare conn)

(defn store-schema []
  (prn (transact conn schema)))

(defn connect []
  (def conn (d/connect cfg)))

(defn setup []
  (d/create-database cfg)
  (connect)
  (store-schema))

(defn add-person [name email]
  (transact conn [{:name name :email email}]))

(defn add-session [name figure reps offset]
  (transact conn [{:yogi {:name name} :figure (trim figure) :reps reps :offset offset}]))

(defn set-color [name color]
  (transact conn [{:db/id [:name name] :color color}]))


(defn persons []
  (q '[:find [?name ...]
       :where [_ :name ?name]]
     @conn))

(defn figures []
  (q '[:find [?fig ...]
       :where
       [_ :figure ?fig]]
     @conn))

(defn sessions []
  (q '[:find ?name ?fig ?reps ?time ?date2 ?y
       :keys name figure reps time date id
       :in $ ?zone-id
       :where
       [?x :name ?name]
       [?y :yogi ?x ?trans]
       [?y :figure ?fig]
       [?y :reps ?reps]
       [(get-else $ ?y :offset 0) ?offset]
       [?trans :db/txInstant ?time]
       [(java-time/local-date ?time ?zone-id) ?date]
       [(java-time/days ?offset) ?days]
       [(java-time/minus ?date ?days) ?date2]]
     @conn (zone-id)))

(defn get-session [id tx-time]
  (let [zi (zone-id)
        e (pull @conn [:figure :reps :offset {:yogi [:name]}] id)
        tx-date (java-time/local-date tx-time zi)]
    (-> (assoc e
          :id id
          :time tx-time
          :name (get-in e [:yogi :name])
          :date (java-time/minus tx-date (java-time/days (:offset e 0))))
        (dissoc :yogi :offset))))

;; This assumes that entities never change, which is true for session entities
(def get-session-m (memoize get-session))

(defn sessions-m []
  (->> (q '[:find ?y ?time
            :in $
            :where
            [?y :yogi ?x ?trans]
            [?trans :db/txInstant ?time]]
          @conn)
       (map (fn [[id tx-time]]
              (get-session-m id tx-time)))))

(defn get-colors []
  (into {} (q '[:find ?name ?color
                :where
                [?yogi :name ?name]
                [?yogi :color ?color]]
              @conn)))

(defn max-reps [fig]
  (q '[:find (max ?reps) .
       :in $ ?fig
       :where
       [?x :figure ?fig]
       [?x :reps ?reps]]
     @conn fig))

(defn get-day [inst]
  (local-date inst (zone-id)))

(defn add-session-page [{:keys [params]}]
  (let [name (if (seq (params "name-override"))
               (params "name-override")
               (params "name"))
        figure (if (seq (params "figure-override"))
                 (params "figure-override")
                 (params "figure"))]
    (doseq [reps (split (params "reps") #"\s+")]
      (add-session name figure (Long. reps) (Long. (params "offset"))))
    (assoc (redirect "/")
      :cookies {:name {:value name}
                :figure {:value figure}})))

(defn make-day [date data names mr]
  (list
   [:tr.date-bar [:td {:colspan 100} date]]
   (let [day (into {} (group-by :name data))
         alldays (into {} (for [[name d] day]
                            [name (apply + (map :reps d))]))
         day-record (apply max (map second alldays))]
     [:tr (for [name names
                :let [ws (day name)
                      ss (reverse (sort-by :time ws))
                      allday (alldays name)]]
            [:td (if (= allday day-record)
                   {:class :day-record})
             (if allday
               [:div.allday allday]
               [:div "-"])
             (for [{:keys [reps id]} ss]
               [:div (if (= reps mr)
                       {:class :max})
                (str reps) (link-to {:class :retract-link} (str "/retract/" id) "x")])])])))

(defn map-values [f m]
  (into {} (for [[k v] m]
             [k (f v)])))

(defn day-view [entries]
  (into {} (for [[date zs] (group-by :date entries)]
             [(time-between date (local-date) :days)
              (apply + (map :reps zs))])))

(def DAILY-GOAL 50)

(defn div [divisor numerator]
  (/ numerator divisor))

(defn magic-streak [days]
  (some->> (sort-by first days)
           (map (fn [[days-ago reps]]
                  [(* (- days-ago 2) DAILY-GOAL) reps]))
           (reductions (fn [[_ reps-after] [goal reps]]
                         [goal (+ reps-after reps)]))
           (take-while (partial apply <))
           last
           second
           (div DAILY-GOAL)
           bigdec))

(defn ema [days]
  (let [weights (map #(Math/pow 0.9 %)
                     (range 100))]
    (int (/ (apply + (map-indexed (fn [days-ago weight]
                                    (* (get days days-ago 0)
                                       weight))
                                  weights))
            (apply + weights)))))

(defn score-row [class names data]
  [:tr {:class class}
   (map (comp (partial vector :td)
              data)
        names)])

(defn make-table [xs mr limit]
  (let [dv (map-values day-view (group-by :name xs))
        ms (map-values magic-streak dv)
        all-time (map-values (comp (partial apply +) vals) dv)
        names (sort-by all-time > (keys all-time))
        colors (get-colors)
        today (local-date)]
    [:table
     (score-row :ema names (map-values ema dv))
     (score-row :magic-streak names (map-values #(some->> % (format "%.1f")) ms))
     (score-row :magic-streak-date names
                (map-values #(some->> %
                                      int
                                      java-time/days
                                      (java-time/minus today))
                            ms))
     (score-row :alltime names all-time)
     [:tr.name-bar (for [yogi names]
            [:td {:style (str "font-weight: bold; color: " (or (colors yogi) "darkgrey"))}
             yogi])]
     (for [[day zs] (take limit (reverse (sort-by first (group-by :date xs))))]
       (make-day day zs names mr))]))

(defn make-bar [xs orientation grouper f]
  (let [sum (apply + (map :reps xs))]
    (for [[group ys] (sort-by first (group-by grouper xs))
          :let [reps (apply + (map :reps ys))]]
      [:div {:style (str orientation ": " (float (* 100 (/ reps sum))) "%")}
       (f group ys)])))

(defn as-week [date]
  (java-time/as date :week-based-year :week-of-week-based-year))

(defn make-graph [xs]
  [:div.graph
   (let [colors (get-colors)
         yogi-f (fn [name _]
                  [:div {:style (str "height: 100%; background-color: "
                                     (or (colors name) "grey"))}
                   "&nbsp;"])
         day-f (fn [_ ys]
                 [:div.graph-horizontal
                  (make-bar ys 'width :name yogi-f)])
         week-f (fn [_ ys]
                  [:div {:style "height: 100%"}
                   (make-bar ys 'height :date day-f)])]
     (make-bar xs 'width (comp as-week :date) week-f))])

(defn make-figure [fig xs limit]
  [:div
   [:div fig (str)]
   (make-graph xs)
   (make-table xs (max-reps fig) limit)])

(defn index [{:keys [cookies]}]
  (html5
    [:title "swole"]
    (include-css "/style.css")
    (form-to {:class :flex} [:post "/add-session"]
      [:div
       "days before" [:br]
       [:input {:type :number
                :id :offset
                :name :offset
                :value 0}]]
      [:div
       "name" [:br]
       [:select {:name :name}
        (select-options (persons) (:value (cookies "name")))]
       [:br]
       (text-field :name-override)]
      [:div
       "figure" [:br]
       [:select {:name :figure}
        (select-options (figures) (:value (cookies "figure")))]
       [:br]
       (text-field :figure-override)]
      [:div
       "reps" [:br]
       (text-field {:inputmode :numeric} :reps)]
      [:div (submit-button :log)])
    [:div.flex
     (for [[fig xs] (sort-by (comp count second) > (group-by :figure (sessions-m)))]
       [:div (make-figure fig xs 14)
        (link-to (str "/f/" (url-encode fig)) "more")])]))

(defn for-figure [figure]
  (html5
    [:title (str figure " - swole")]
    (include-css "/style.css")
    (make-figure figure ((group-by :figure (sessions-m)) figure) 9999)))

(defn get-retract [id]
  (html5
    [:title (str "swole - retract " id)]
    (include-css "/style.css")
    [:div "do you want to retract?"]
    (str (pull @conn [:* {:yogi [:name]}] id))
    (form-to {:class :flex} [:post "/retract"]
      (hidden-field :id id)
      (link-to "/" "no")
      (submit-button :yes))))

(defn post-retract [{:keys [params]}]
  (transact conn [[:db/retractEntity (Long. (params "id"))]])
  (redirect "/"))

(defroutes app
  (GET "/" [] index)
  (GET "/f/:figure" [figure] (for-figure (url-decode figure)))
  (POST "/add-session" [] add-session-page)
  (GET "/retract/:id" [id] (get-retract (Long. id)))
  (POST "/retract" [] post-retract)
  (route/resources "/")
  (route/not-found "not found"))

(def wrapped-app (-> app
                     wrap-params
                     wrap-cookies))

(def reloadable-app
  (wrap-reload #'wrapped-app))

(defn -main []
  (connect)
  (run-jetty reloadable-app {:port 3000 :join? false}))

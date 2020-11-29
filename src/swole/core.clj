(ns swole.core
  (:require [datahike.api :refer [q pull db transact] :as d]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.cookies :refer [wrap-cookies]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.util.response :refer [redirect]]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [hiccup.page :refer [html5 include-css]]
            [hiccup.form :refer [form-to text-field submit-button select-options]]
            [java-time :refer [local-date zone-id]]))

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
             (one :yogi :db.type/ref)
             (one :reps :db.type/long)
             (one :figure :db.type/string)
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

(defn add-session [name figure reps]
  (transact conn [{:yogi {:name name} :figure figure :reps reps}]))

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
  (q '[:find ?name ?fig ?reps ?time
       :keys name figure reps time
       :where
       [?x :name ?name]
       [?y :yogi ?x ?trans]
       [?y :figure ?fig]
       [?y :reps ?reps]
       [?trans :db/txInstant ?time]]
     @conn))

(defn get-day [inst]
  (local-date inst (zone-id)))

(defn add-session-page [{:keys [params]}]
  (let [name (if (seq (params "name-override"))
               (params "name-override")
               (params "name"))
        figure (if (seq (params "figure-override"))
                 (params "figure-override")
                 (params "figure"))]
    (add-session name figure (Long. (params "reps")))
    (assoc (redirect "/")
      :cookies {:name {:value name}
                :figure {:value figure}})))

(defn make-table [xs]
  (let [bla (sort-by first (group-by :name xs))]
    [:table
     [:tr (for [[_ ys] bla]
            [:td.alltime (apply + (map :reps ys))])]
     [:tr (for [[yogi _] bla]
            [:td yogi])]
     (for [[day zs] (reverse (sort-by first (group-by #(get-day (:time %)) xs)))]
       (list
        [:tr [:td {:colspan 100} day]]
        [:tr (for [[_ ws] (sort-by first (group-by :name zs))]
               (let [ss (map :reps (reverse (sort-by :time ws)))]
                 [:td [:div.allday (apply + ss)]
                  (for [s ss]
                    [:div (str s)])]))]))]))

(defn index [{:keys [cookies]}]
  (html5
    [:title "swole"]
    (include-css "/style.css")
    (form-to {:class :flex} [:post "/add-session"]
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
       (text-field :reps)]
      (submit-button :log))
    [:div.flex
     (for [[fig xs] (sort-by (comp count second) > (group-by :figure (sessions)))]
       [:div
        [:div fig (str)] (make-table xs)])]))

(defroutes app
  (GET "/" [] index)
  (POST "/add-session" [] add-session-page)
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

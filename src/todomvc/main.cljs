(ns todomvc.main
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [cljs.reader :as rdr]
            [malli.core :as m]
            [malli.error :as me]
            [re-frame.core :as rf]
            [re-frame.alpha :as rfa]
            [re-frame.db :refer [app-db]]
            [reagent.core :as r]
            [reagent.dom.client :as rdc]
            [reitit.core :as reit-c]
            [reitit.coercion :as coercion]
            [reitit.coercion.malli :as reit-cm]
            [reitit.frontend :as reit-f]
            [reitit.frontend.easy :as reit-fe]
            ["id128" :as id128]))

;; Utilities

(defn ulid
  "Returns a UUID-formatted [ULID](https://github.com/ulid/spec)."
  []
  (let [ulid (str/lower-case (.toRaw (id128/UlidMonotonic.generate)))
        a    (subs ulid 0 8)
        b    (subs ulid 8 12)
        c    (subs ulid 12 16)
        d    (subs ulid 16 20)
        e    (subs ulid 20 32)]
    (uuid (str/join "-" [a b c d e]))))

(defn check-and-throw
  "Throws an exception if `db` does not match `schema`."
  [schema db]
  (when-not (m/validate schema db)
    (throw
     (ex-info (str "Schema check failed: " (me/humanize (m/explain schema db)))
              {}))))

(defn valid? [schema]
  (m/validator schema))

(def ls-key "rf-todos")

(defn todos->local-store
  "Stores `todos` in localStorage"
  [todos]
  (js/localStorage.setItem ls-key (str todos)))

;; App database (data layer)

(def db-schema
  [:map 
   [:showing [:enum :all :done :active]]
   [:todos [:map-of
            :uuid
            [:map
             [:id uuid?]
             [:title string?]
             [:done boolean?]]]]])

(def default-db {:todos   (sorted-map)
                 :showing :all
                 :very    {:long {:path {:alpha? false}}}})

;; Subscriptions (query layer)

(rf/reg-sub
 :todos
 ;; Signal function
 (fn [query-v _]
   (rf/subscribe [:sorted-todos]))

 ;; Computation function
 (fn [sorted-todos query-v _]
   (vals sorted-todos)))

(defn sorted-todos [db _]
  (:todos db))

(rf/reg-sub
 :sorted-todos
 sorted-todos)

(rf/reg-sub
 :showing
 (fn [db _]
   (:showing db)))

(rf/reg-sub
 :visible-todos
 (fn [_ _]
   [(rf/subscribe [:todos])
    (rf/subscribe [:showing])])

 (fn [[todos showing] _]
   (let [filter-fn (case showing
                     :active (complement :done)
                     :done   :done
                     :all    identity)]
     (filter filter-fn todos))))

(rf/reg-sub
 :all-complete?
 :<- [:todos]
 (fn [todos _]
   (every? :done todos)))

(rf/reg-sub
 :completed-count
 :<- [:todos]
 (fn [todos _]
   (count (filter :done todos))))

(rf/reg-sub
 :footer-counts
 :<- [:todos]
 :<- [:completed-count]
 (fn [[todos completed] _]
   [(- (count todos) completed) completed]))

(rfa/reg :sub
         :alpha? 
         :-> (comp :alpha? :path :long :very))

;; View components (view layer)

(defn todo-input [{:keys [title on-save on-stop]}]
  (let [val (r/atom title)
        stop #(do (reset! val "")
                  (when on-stop (on-stop)))
        save #(let [v (-> @val str str/trim)]
                (on-save v)
                (stop))]
    (fn [props]
      [:input (merge (dissoc props :on-save :on-stop :title)
                     {:type        "text"
                      :value       @val
                      :auto-focus  true
                      :on-blur     save
                      :on-change   #(reset! val (-> % .-target .-value))
                      :on-key-down #(case (.-which %)
                                      13 (save)
                                      27 (stop)
                                      nil)})])))

(defn todo-item []
  (let [editing (r/atom false)]
    (fn [{:keys [id done title]}]
      [:li {:class (str/join " " [(when done "completed")
                                  (when @editing "editing")])}
       [:div.view
        [:input.toggle {:type      "checkbox"
                        :checked   done
                        :on-change #(rf/dispatch [:toggle-done id])}]
        [:label {:on-double-click #(reset! editing true)}
         title]
        [:button.destroy {:on-click #(rf/dispatch [:delete-todo id])}]]
       (when @editing
         [todo-input {:class   "edit"
                      :title   title
                      :on-save #(if (seq %)
                                  (rf/dispatch [:update-todo id %])
                                  ; Does this handle the case w/o a new title?
                                  (rf/dispatch [:delete-todo id])) 
                      :on-stop #(reset! editing false)}])])))

(defn task-list []
  (let [visible-todos @(rf/subscribe [:visible-todos])
        all-complete? @(rf/subscribe [:all-complete?])]
    [:section#main
     [:input#toggle-all {:type      "checkbox"
                         :checked   all-complete?
                         :on-change #(rf/dispatch [:complete-all-toggle])}]
     [:label {:for "toggle-all"}
      "Mark all as complete"]
     [:ul#todo-list
      (for [todo visible-todos]
        ^{:key (:id todo)}
        [todo-item todo])]]))

(defn task-entry []
  [:header#header
   [:h1 "dodos"]
   [todo-input {:id "new-todo"
                :placeholder "What's to do, Scooby-Doo?"
                :on-save #(when (seq %) (rf/dispatch [:create-todo %]))}]])

(defn footer-controls []
  (let [[active done] @(rf/subscribe [:footer-counts])
        showing       @(rf/subscribe [:showing])
        a-fn          (fn [filter-kw text]
                        [:a {:class (when (= filter-kw showing) "selected") 
                             :href  (str "/" (name filter-kw))}
                         text])]
    [:footer#footer
     [:span#todo-count
      [:strong (str/join " " [active (case active 1 "item" "items") "left"])]]
     [:ul#filters
      [:li (a-fn :all "All")]
      [:li (a-fn :active "Active")]
      [:li (a-fn :done "Completed")]]
     (when (pos? done)
       [:button#clear-completed {:on-click #(rf/dispatch [:clear-completed])}
        "Clear completed"])]))

(defn alpha []
  (let [alpha? @(rf/subscribe [:alpha?])]
    [:a {:href     "#"
         :style    {:color (if alpha? "blue" "gray")}
         :on-click #(rf/dispatch [:toggle-alpha])}
     (str "`re-frame.alpha/sub` instance value: " (if alpha? "`true`" "`false`"))]))

(defn todo-app []
  [:<>
   #_[alpha] ;; Uncomment to display
   [:section#todoapp
    [task-entry]
    (when (seq @(rf/subscribe [:todos]))
      [task-list])
    [footer-controls]]
   [:footer#info
    [:p "Double click to edit a todo"]]])

;; Control/update layer: event interceptors and events

;; Event interceptors

(def check-schema-interceptor (rfa/after (partial check-and-throw db-schema)))

(def ->local-store (rfa/after todos->local-store))

(def todo-interceptors [check-schema-interceptor
                        (rfa/path :todos)
                        ->local-store])

;; Events

(rf/reg-event-fx
 :initialize-db
 [(rf/inject-cofx :local-store-todos)
  check-schema-interceptor]
 (fn [{:keys [ls-todos]} _]
   {:db (assoc default-db
               :todos
               ls-todos)}))

(rf/reg-event-db
 :toggle-done
 todo-interceptors
 (fn [todos [_ id]]
   (update-in todos [id :done] not)))


(rf/reg-event-db
 :create-todo
 todo-interceptors
 (fn [todos [_ title]]
   (let [id (ulid)]
     (assoc todos id {:id id :title title :done false}))))

(rf/reg-event-db
 :update-todo
 todo-interceptors
 (fn [todos [_ id title]]
   (assoc-in todos [id :title] title)))

(rf/reg-event-db
 :delete-todo
 todo-interceptors
 (fn [todos [_ id]]
   (dissoc todos id)))

(rf/reg-event-db
 :complete-all-toggle
 todo-interceptors
 (fn [todos _]
   (let [new-status (not-every? :done (vals todos))]
     (reduce (fn [tasks t-id]
               (assoc-in tasks [t-id :done] new-status))
             todos
             (keys todos)))))

(rf/reg-event-db
 :set-showing
 [check-schema-interceptor]
 (fn [db [_ new-filter-criteria]]
   (assoc db :showing new-filter-criteria)))

(rf/reg-event-db
 :clear-completed
 todo-interceptors
 (fn [todos _]
   (let [done-ids (->> (vals todos)
                       (filter :done)
                       (map :id))]
     (reduce dissoc todos done-ids))))

(rf/reg-event-db
 :toggle-alpha
 (fn [db _]
   (update-in db [:very :long :path :alpha?] not)))

;; Coeffects

(rf/reg-cofx
 :local-store-todos
 (fn [coeffect _]
   (assoc coeffect 
          :ls-todos
          (into (sorted-map)
                (some->> (js/localStorage.getItem ls-key)
                         (rdr/read-string))))))

;; Routing

(def routes
  ["/"
   [""]
   [":filter"
    {:parameters {:path [:map [:filter keyword?]]}}]])

(def router (reit-f/router routes {:data {:coercion reit-cm/coercion}}))

(defn on-navigate [match _]
  (let [coerced (coercion/coerce! match)]
    (if (not (nil? coerced))
      (rf/dispatch [:set-showing (-> coerced :path :filter)])
      (rf/dispatch [:set-showing :all]))))

(defn init-routes! []
  (js/console.log "Initializing routes")
  (reit-fe/start! router on-navigate {:use-fragment false}))

;; Reagent and development setup

  (def functional-compiler (r/create-compiler {:function-components true}))

  (defonce app-root
    (rdc/create-root (js/document.getElementById "app")))

  (defn render []
    (rdc/render app-root
                [todo-app]
                functional-compiler))

  (defn ^:dev/before-load stop! []
    (js/console.log "Stop"))

  (defn ^:dev/after-load start! []
    (js/console.log "Start")
    (rf/clear-subscription-cache!)
    (render))

  (defn ^:export init! []
    (js/console.log "Initialize")
    (rf/dispatch-sync [:initialize-db])
    (init-routes!)
    (start!))

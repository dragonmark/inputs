(ns ^:figwheel-always dragonmark.inputs.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [cljs.core.async :refer [chan put! >! <! alts! close!]]
   [cljs.core.async.impl.channels :refer [ManyToManyChannel]]
   [schema.core :as s :include-macros ]
   [schema.coerce :as coerce]
   [clojure.string :as str]
   [dragonmark.inputs.utils :refer [full-name ->int]]
   [dragonmark.inputs.extern :refer [get-state set-state! update-state!
                             get-node create-component get-i18n-info
                             build-component]]
   [dragonmark.inputs.date-utils :as d]
   [dragonmark.inputs.schema-utils :as su :refer [sch-type]]
   [dragonmark.inputs.schemas :refer [sch-business-state sch-field-state SchOptions]]
   [dragonmark.inputs.validation :as va]
   [dragonmark.inputs.i18n :as i :refer [comp-i18n label desc desc? data error ph info]]
   [dragonmark.inputs.typing-controls :refer [build-typing-control]]
   [jkkramer.verily :as v]
   [goog.events]
            [sablono.core :as html :refer-macros [html]]))

(enable-console-print!)

(def ^:dynamic *component* nil)

(defn dispatch
  [f v]

  (cond
    (nil? f)
    nil


    (fn? f) (f v)

    (ifn? f) (f v)

    (satisfies? ManyToManyChannel f)
    (put! f v)

    (instance? ManyToManyChannel f)
    (put! f v)

    :else nil
    ))

(defn cc
  ([] nil)
  ([a] a)
  ([a b] (cond
           (and (nil? a) (nil? b))
           nil

           (nil? a)
           (if (sequential? b) b [b])

           (nil? b)
           (if (sequential? a) a [a])

           (and (sequential? a)
                (sequential? b))
           (concat a b)

           (sequential? a) (concat a [b])

           (sequential? b) (concat [a] b)

           :else [a b]
           ))
  ([a b & rst] (apply cc (cc a b) rst))
  )

;_________________________________________________
;                                                 |
;          Events Utils                           |
;_________________________________________________|


(defn e-value
  "Get value from an event"
  [e]
  (-> e .-target .-value))

(defn e-checked
  "Get the checked status of a checkbox."
  [e]
  (-> e .-target .-checked))

;_________________________________________________
;                                                 |
;          Component Utils                        |
;_________________________________________________|

(defn styles
  [& args]
  (str/join " " args))

;___________________________________________________________
;                                                           |
;          Multimethod to handle different inputs form      |
;___________________________________________________________|


(defmulti magic-input
  (fn [{opts :opts}]
    (get-in opts [:type] (sch-type (:k-sch opts)))))

(defn enum-label
  "Display the i18n label of a select or radio group entry, fall backs to the code."
  [i18n code]
  (get-in i18n [:data code :label] (if (keyword? code) (full-name code) code)))

(defn choose-iterator
  "Choose if we iterate on the values of the schema enum or on the keys of the i18n data."
  [{:keys [i18n k-sch label-order]}]
  (if-let [data (and label-order (get-in i18n [:data]))]
       (keys data)
       (:vs k-sch)))

(defmethod magic-input "enum"
  [{{:keys [attrs i18n] :as options} :opts}]

  (into
   [:select attrs
    [:option {:value ""} ""]]
   (map (fn [code]
          [:option {:value code}
           (enum-label i18n code)])
        (choose-iterator options))))


(defn radio-group
  [style {{:keys [attrs k i18n] :as options} :opts chan :chan}]
  [:div {:className "input-group"}
   (into []
         (map (fn [code]
                [:div {:className style}
                 [:label
                  [:input
                   (merge attrs {:type      "radio"
                                 :checked   (= code (:value attrs))
                                 :className ""

                                 :value     code
                                 :onClick   #(do
                                               (dispatch chan [k code])
                                               nil)})]
                   (enum-label i18n code)]])
              (choose-iterator options)))])

(defmethod magic-input "radio-group"
  [m]
  (radio-group "radio" m))

(defmethod magic-input "radio-group-inline"
  [m]
  (radio-group "radio-inline" m))


(defn make-segmented
  "HOF, generates a function,  that closes over the value, for segemented control"
  [type k value i18n chan]
  (fn [code]
    [:button  {:type      type
               :active    (= code value)
               :className (styles "btn" (if (= code value) "btn-primary" "btn-default"))
               :key       (str (full-name k) "_" code)
               :id        (str (full-name k) "_" code)
               :value     code
               :onChange  #(-> nil)
               :onClick   #(do
                             (dispatch chan [k code])
                             (dispatch chan [:validate k])

                             nil)}
               (enum-label i18n code)]))

(defmethod magic-input "btn-group"
  [{{:keys [attrs k i18n] :as options} :opts chan :chan}]
  [:div  (merge attrs {:className "btn-group"})
   (map
    (make-segmented "button" k (:value attrs) i18n chan)
    (choose-iterator options))])

(defmethod magic-input "range-btn-group"
  [{{:keys [attrs i18n k]} :opts chan :chan}]
  (let [{:keys [min max step value] :or {step 1}} attrs]
    [:div (merge attrs {:className "btn-group"})
     (map
      (make-segmented "button" k value i18n chan)
      (range (int min) (inc (int max)) step))]))

(defmethod magic-input "stepper"
  [{{:keys [attrs k]} :opts chan :chan}]
  (let [{:keys [min max step value size]} attrs
        value (->int value)
        min (->int min)
        max (->int max)
        step (->int step)
        plus (if step (partial + (long step)) inc)
        minus (if step (partial + (- (long step))) dec)
        style (styles "btn btn-default" (when size (str "btn-" size)))]
    [:div (merge attrs {:className "btn-group stepper"})
     [:button {:type      "button"
               :className style
               :onClick   #(when (or (nil? min)
                                     (and min (<= (int min) (minus value))))
                             (dispatch chan [k (str (minus value))])
                             (dispatch chan [:validate k])
                             nil)} "-"]
     [:input {:className "input-stepper"
              :size      (if (str/blank? value) 1 (count (str value)))
              :onChange  #(-> nil)
              :value     value}]

     [:button {:type      "button"
               :className style
               :onClick   #(when (or (nil? max)
                                     (and max (<= (plus value) (int max))))
                             (dispatch chan [k (str (plus value))])
                             (dispatch chan [:validate k])
                             nil)} "+"]]))


(defmethod magic-input s/Inst
  [{{:keys [k attrs]} :opts chan :chan}]
  (let [date (:value attrs)]
    [:input (merge attrs
                   {:placeholder d/default-fmt
                    :value       (d/display-date date)
                    :onChange    #(do
                                    (dispatch chan [k (e-value %)])
                                    (dispatch chan [:validate k])
                                    nil)
                    :onBlur      #(let [v (e-value %)]
                                    (dispatch chan [k (when-not (str/blank? v) (d/parse v))])
                                    (dispatch chan [:focus k])
                                    (dispatch chan [:validate k])
                                    nil)})]))

(defmethod magic-input "date"
  [{{:keys [k attrs]} :opts chan :chan}]
  (let [date (:value attrs)]
    [:input (merge attrs
                   {:type     "date"
                    :value    (d/display-date "yyyy-MM-dd" date)
                    :onChange #(do
                                 (dispatch chan [k (e-value %)])
                                 (dispatch chan [:validate k])
                                 nil)
                    :onBlur   #(let [v (e-value %)]
                                 (dispatch chan [k (when-not (str/blank? v) (d/parse "yyyy-MM-dd" v))])
                                 (dispatch chan [:focus k])
                                 (dispatch chan [:validate k])
                                 nil)})]))


(defmethod magic-input s/Bool
  [{{:keys [k attrs]} :opts chan :chan}]
  (let [value (:value attrs)]
    [:input (merge attrs {:checked  (js/Boolean value)
                          :onChange #(do
                                       (dispatch chan [k (-> % .-target .-checked)])
                                       (dispatch chan [:validate k])
                                       nil)
                          :type     "checkbox"})]))


(defmethod magic-input "range"
 [{{:keys [attrs]} :opts}]
 [:input (merge {:type "range"} attrs)])

(defmethod magic-input "now"
  [{{:keys [attrs k]} :opts chan :chan}]
  [:input (merge attrs {:type           "button"
                        :className      "btn"
                        :preventDefault  true
                        :onClick        #(do
                                           (dispatch chan [k (js/Date.)])
                                           (dispatch chan [:validate k])
                                           nil)})])


(defmethod magic-input "email"
  [{{:keys [attrs]} :opts}]
  [:input (merge  {:type "email"
                   :autoCapitalize "off"
                   :autoCorrect "off"} attrs)])

(defmethod magic-input "password"
  [{{:keys [attrs]} :opts}]
  [:input (merge  {:type "password"} attrs)])


(defmethod magic-input :default
  [{{:keys [attrs]} :opts}]
  [:input attrs])




;_________________________________________________
;                                                 |
;          Component Initialisation               |
;_________________________________________________|


(defprotocol Required
  (required? [this]))

(extend-protocol Required
  schema.core.OptionalKey
  (required? [this]
             false)
  schema.core.RequiredKey
  (required? [this]
             true)
  Keyword
  (required? [k]
             true))



(defn init-state-value
  "Choose the initial value.
   s/Num fields must be represented as string because of the typing control.
  If an initial value is provided for a s/Num it will be represented as a string in the
  local state."
  [v t]
  (condp = t
    s/Num (str v)
    v))


(s/defn  build-init-state :- sch-business-state
  "Build the initial business local state backing the inputs in the form.
   It accepts init values from the options
  TODO validate that the value is correct"
  ([sch
    init]
   (into {} (for [[k t] sch
                  :let [fk (get k :k k)
                        t (sch-type t)
                        init-val (get init fk "")]]
              [fk {:value (init-state-value init-val t)
                   :required (required? k)
                   :type t}])))
  ([sch]
   (build-init-state sch {})))


;___________________________________________________________
;                                                           |
;                 Decorate s/inst fields                    |
;___________________________________________________________|



(defn add-date-picker!
  "Decorate an HTML node with google.ui.inputdatepicker"
  [k node chan f]
  (let [dp (d/date-picker f)]
    (.decorate dp node )
    (goog.events/listen dp goog.ui.DatePicker.Events.CHANGE
                        #(do
                           (dispatch chan [k  (d/goog-date->js-date (.-date %))])
                           (dispatch chan [:validate k])
                           nil))))

(defn handle-date-fields!
  [owner f opts]
  (let [chan (get-state owner :chan)
        state (get-state owner :inputs)
        date-fieds (for [[k {:keys [type]}] state
                         :when (and (= s/Inst type)
                                    (not (#{"now" "date"} (get-in opts [k :type]))))]
                     k)]
    (doseq [k date-fieds]
      (add-date-picker! k (get-node owner (full-name k)) chan f))))



;___________________________________________________________
;                                                           |
;                 React Sub-Components                      |
;___________________________________________________________|


(defn default-tooltip
  "Display a tooltip next to the field to inform the user.
  options :
  :k the target field
  :type serves to build the css class tooltip-type
  :action attach a function when closing the tooltip"
  [app owner opts]
  (let [the-id (str (name (:k opts)) "-tooltip")]
    (create-component
      {:component-did-mount
       (fn
         [this]
         (let [tool (.getElementById js/document the-id)
               elem (.getElementById js/document (full-name (:k opts)))
               rect-tool (.getBoundingClientRect tool)
               rect (.getBoundingClientRect elem)
               delta (* 0.5 (- (.-height rect) (.-height rect-tool)))]
           (set! (.-left (.-style tool)) (str (+ 5 (.-width rect)) "px"))
           (set! (.-top (.-style tool)) (str delta "px"))))
       :render
       (fn
         [_]
         [:div {:className (styles "popover right" (str "popover-" (:type opts)))
                :role "alert"
                :id the-id
                :ref (str (name (:k opts)) "-tooltip")}
          [:div {:className "arrow"} ""]
          (when (:title app)  [:div {:className "popover-title"} (:title app)])
          [:div {:className "popover-content"} (:mess app)
           [:div {:type "button"
                  :className "close"
                  :onClick (:action opts)} "x"]]])})) [app owner opts]
  (let [the-id (str (name (:k opts)) "-tooltip")]
    (create-component
     {:component-did-mount
      (fn
        [this]
        (let [tool (.getElementById js/document the-id)
              elem (.getElementById js/document (full-name (:k opts)))
              rect-tool (.getBoundingClientRect tool)
              rect (.getBoundingClientRect elem)
              delta (* 0.5 (- (.-height rect) (.-height rect-tool)))]
          (set! (.-left (.-style tool)) (str (+ 5 (.-width rect)) "px"))
          (set! (.-top (.-style tool)) (str delta "px"))))
      :render
      (fn
        [_]
        [:div {:className (styles "popover right" (str "popover-" (:type opts)))
               :role "alert"
               :id the-id
               :ref (str (name (:k opts)) "-tooltip")}
         [:div {:className "arrow"} ""]
         (when (:title app)  [:div {:className "popover-title"} (:title app)])
         [:div {:className "popover-content"} (:mess app)
          [:div {:type "button"
                 :className "close"
                 :onClick (:action opts)} "x"]]])})))

(def ^:dynamic *tooltip-func* nil)



(def tooltip-atom (atom default-tooltip))

(defn tooltip
  []
  (or
    *tooltip-func*
    @tooltip-atom))


(defn message
  "Display a dismissable error message"
  [app owner m]
  (create-component
   {:render
    (fn
      [this]
      (let [{:keys [chan mess ] :as state} (get-state this)]
        [:div {:className "alert alert-danger"
               :role "alert"}
         [:button {:type "button"
                   :className "close"
                   :data-dismiss "alert"
                   :onClick #(do
                               (dispatch chan [:kill-mess (:k m)])
                               nil
                               )} "x"]
         mess]))}))

(defn description
  "Display a small description under the label"
  [app owner m]
  [:div {:className "description"} (:desc m)])


(defn button-view
  [app owner {:keys [k labels comp-name attrs] :as opts}]
  (create-component
   {:render
    (fn [this]
      (let [state (get-state owner)
            chan-name (keyword (str (name k) "-chan"))
            chan (get-state owner chan-name)
            button-state (get-in state [:action-state k])
            btn-style (when button-state (name button-state))]
        [:div
         [:button (merge attrs {:type      "button"
                                :id        (str (full-name comp-name) "-" (name k))
                                :disabled  (boolean btn-style)
                                :className (styles "btn btn-primary has-spinner has-error" btn-style)
                                :onClick   #(do
                                             (dispatch chan k)
                                              nil)})
          (label labels k opts)
          [:span {:className "error"}
           [:i {:className "fa fa-ban text-danger"}]]
          [:span {:className "spinner"}
           [:i {:className "fa fa-spin fa-cog"}]]]
         [:div {:className "description"} (:desc labels)]]))}))

;___________________________________________________________
;                                                           |
;        Syntactic sugar to access business state           |
;___________________________________________________________|

(s/defn get-in-bs
            [m :- sch-business-state
             f :- s/Keyword
             k :- s/Keyword]
            (get-in m [f k]))


(s/defn fvalue :- s/Any
        [m k]
        (get-in-bs m k :value))

(s/defn fvalid :- (s/maybe s/Bool)
        [m k]
        (get-in-bs m k :valid))

(s/defn frequired :- s/Bool
        [m k]
            (get-in-bs m k :required))

(s/defn ferrors :- va/sch-errors-list
        [m k]
        (get-in-bs m k :error))

(s/defn fdisabled :- (s/maybe s/Bool)
  [m k]
  (get-in-bs m k :disabled))

(defn ffocus
  [m k]
  (get-in-bs m k :focus))

;___________________________________________________________
;                                                           |
;        Business State Manipulation                        |
;___________________________________________________________|


(s/defn assoc-in-all :- sch-business-state
  [bs :- sch-business-state
   k :- s/Keyword
   v :- s/Any]
  (into {}
        (for [[fk s] bs]
          {fk (assoc s k v)})))


(defn disable-all
  "Disable all inputs after successful validation."
  [bs]
  (assoc-in-all bs :disabled true))

(defn enable-all
  "Disable all inputs after successful validation."
  [bs]
  (assoc-in-all bs :disabled false))


;___________________________________________________________
;                                                           |
;                 React Form Component Builders             |
;___________________________________________________________|


(defn error-mess
  "Finds the i18n message for the first error on a field."
  [owner kbs lang opts]
  (let [full-i18n (get-i18n-info owner [:i18n lang])]
    (when-let [[err-k & errs] (cc (:error kbs) (:async-error kbs))]
      (i/error full-i18n err-k opts))))

(defn validation-style
  [{:keys [valid invalid]}]
  (cond valid "has-success" invalid "has-error has-feedback" :else ""))

(defn required-style
  [kbs]
  (if (:required kbs) "required" "optional"))



(defmulti layout-input
          (fn [_ {layout :layout} _ _]
            layout))

(defmethod layout-input :default
  [owner opts kbs {:keys [invalid] :as val-states}]
  (let [{:keys [chan lang]} (get-state owner)]
    [:div {:className (styles "form-group" (validation-style val-states))}
     [:label {:htmlFor   (full-name (:k opts))
              :className (styles "control-label" (required-style kbs))}
      (i/label opts)]
     (when (:labeled opts) [:label {:className "badge"} (:value kbs)])
     (when (i/desc opts) [:div {:className "description"} (i/desc opts)])
     (when (i/html-desc opts) (html (i/html-desc opts)))
     [:div {:className "input-container"}
      (let [opts (assoc-in opts [:attrs :className] "form-control")]
        (magic-input {:chan chan :opts opts}))
      (when (and (i/info opts)
                 (:focus kbs))
        [(build-component (tooltip) {:mess  (i/info opts)
                                   :title (i/info-title opts)} owner
                          {
                           :opts {:k (:k opts) :type "info"}})])
      (let [mess (error-mess owner kbs lang opts)]
        (when (and invalid mess)
          [(build-component (tooltip) {:mess mess} owner
                            {:state  {:mess mess}
                             :opts  {:k      (:k opts)
                                     :type   "error"
                                     :action #(do
                                                (dispatch chan [:kill-mess (:k opts)])
                                                nil)}})]))]]))

(defmethod layout-input "in-line"
  [owner opts kbs {:keys [invalid] :as val-states}]
  (let [{:keys [chan lang]} (get-state owner)]
    [:div {:className (validation-style val-states)}
     [:div {:className (styles "checkbox" (required-style kbs))}
      [:label {:htmlFor   (full-name (:k opts))}
       (when (:labeled opts) [:label {:className "badge"} (:value kbs)])
       [:div {:className "input-container"}
        (magic-input {:chan chan :opts opts})
        (when (and (i/info opts)
                   (:focus opts))
          [(build-component (tooltip) {:mess  (i/info opts)
                                     :title (i/info-title opts)} owner
                                     {:state {:mess  (i/info opts)
                                              :title (i/info-title opts)}
                                      :opts {:k (:k opts) :type "info"}})])]
       [:div (i/label opts)]]
      (let [mess (error-mess owner kbs lang opts)]
        (when (and invalid mess)
          [(build-component (tooltip) {:mess mess} owner
                            {:state  {:mess mess}
                             :opts  {:k      (:k opts)
                                     :type   "error"
                                     :action #(do
                                                (dispatch chan [:kill-mess (:k opts)])
                                                nil)}})]))]
     (when (i/desc opts) [:p {:className "description"} (i/desc opts)])
     (when (i/html-desc opts) (html (i/html-desc opts)))]))


(defn build-input
  [owner {:keys [k] :as opts}]
  (let [{:keys [chan inputs]} (get-state owner)
        kbs (k inputs)
        valid (:valid kbs)
        controled (not (nil? valid))
        invalid (and controled (not valid))
        val-states {:valid     valid
                    :invalid   invalid
                    :controled controled}
        k-attrs {:id          (full-name k)
                 :key         (full-name k)
                 :ref         (full-name k)
                 :value       (:value kbs)
                 :onBlur      (fn [_]
                                (dispatch chan [:focus k])
                                (dispatch chan [:validate k])
                                nil)
                 :onFocus     (fn [_]
                                 (dispatch chan [:focus k])
                                 nil)
                 :onChange    #(do
                                 (dispatch chan [k (e-value %)])
                                 (dispatch chan [:validate k])
                                 nil)
                 :placeholder (get-in opts [:i18n :ph])
                 :disabled    (:disabled  kbs)}
        opts (update-in opts [:attrs] #(merge k-attrs %))]
    (layout-input owner opts kbs val-states)))



(def action-states
  {:init :active
   :in-error :active
   :active :disabled
   :disabled :init})

(def error-flow
  {:init :in-error
   :active :in-error
   :disabled :disabled
   :in-error :in-error})

(defn run-exec
  [obj cmds]

      (doseq
        [[cmd & rst] cmds]
        (case
          cmd

          :action-state
          (set-state! obj [:action-state :action] (first rst))

          :remove-error
          (update-state! obj [:inputs]
                         (fn [m]
                           (let [fld (first rst)
                                 cv (dissoc (fld m) :async-error)
                                 cv (update cv :valid #(let [e (:error %)]
                                                        (not
                                                          (boolean
                                                            (or (nil? e)
                                                                (empty? e))))))
                                 ]
                             (assoc m fld cv))))

          :add-error
          (let [[fld errs] rst]
            (when (some-> obj get-state :inputs fld)
              (set-state! obj [:action-state :action] :in-error)
              (update-state! obj [:inputs fld]
                             #(-> %
                                  (assoc :valid false)
                                  (update :async-error cc errs)))))

          :assoc-in
          (set-state! obj (first rst) (second rst))

          :dissoc-in
          (let [f (first rst)
                v (-> f butlast vec)
                i (last f)]
               (update-state! obj v #(dissoc % i)))))

  )

(defn do-meta-validation
  [this]
  (let [{:keys [inputs] :as state} (get-state this)
        new-bs (va/full-validation inputs state)
        no-error (va/no-error? new-bs)]

    (if no-error
      (set-state! this [:action-state :action] nil)
      (set-state! this [:action-state :action] :in-error))
    (set-state! this [:inputs] new-bs)
    no-error)
  )

(s/defn
  make-input-comp
  "Build an input form Om component based on a prismatic/Schema"
  ([{:keys [name schema action clean opts] :or {opts {}} :as spec}]
    (make-input-comp name schema action clean opts))
  ([comp-name :- s/Keyword
    schema
    action]
   (make-input-comp comp-name schema action nil {}))
  ([comp-name :- s/Keyword
    schema
    action
    opts]
  (make-input-comp comp-name schema action nil opts))
  ([comp-name :- s/Keyword
    schema
    action
    clean
    opts :- SchOptions]
   (let [order (:order opts)
         ext-chan (:ext-chan opts)
         verily-rules (:validations opts)

         mount-info-atom (:mount-info-atom opts)
         schema-coercer (coerce/coercer schema va/validation-coercer)
         validation (va/build-verily-validator verily-rules)
         unit-coercers (va/build-unit-coercers schema)
         unit-validators (va/unit-schema-validators unit-coercers)
         remove-errs-fn (va/build-error-remover verily-rules va/inter-fields-rules)
         typing-controls (build-typing-control schema)
         initial-bs (build-init-state schema (:init opts))
         initial-action-state {:action :disabled
                               :clean :disabled}
         close-channels (fn [this]
                          (let [{:keys [chan
                                        action-chan
                                        created-chan
                                        clean-chan
                                        ]} (get-state this)]
                            (close! action-chan)
                            (close! created-chan)
                            (close! clean-chan)
                            ))
         ]
     (fn [app]
       (create-component
         {:get-initial-state
          (fn [this]
            {:opts             opts
             :chan             (fn [[k v]]
                                 (binding [*component* this]
                                   (condp = k
                                     :focus (do
                                              ;; (js/setTimeout #(update-state! this [:inputs v :focus] not) 500)
                                              nil)
                                     :kill-mess (update-state! this [:inputs v] #(dissoc % :error))
                                     :validate (do
                                                 (va/field-validation! this v)
                                                 (do-meta-validation this))

                                     :update-input
                                     (do
                                       (run-exec this v))

                                     (let [coerce (get typing-controls k (fn [n _] n))
                                           ptfn (get-in opts [k :post-typing] identity)
                                           v (ptfn v)
                                           old-val (get-state this [:inputs k :value])
                                           coerced (coerce v old-val)]
                                       (set-state! this [:inputs k :value] coerced)))))

             :action-chan      (chan 10)
             :created-chan     (chan 10)
             :clean-chan       (chan 10)
             :action-state     initial-action-state
             :inputs           initial-bs
             :unit-coercers    unit-coercers
             :unit-validators  unit-validators
             :verily-validator validation
             :remove-errs-fn   remove-errs-fn
             :validation-deps  (va/fields-dependencies verily-rules)})

          :component-will-mount
          (fn [this]
            (binding [*component* this]
              (when ext-chan (dispatch ext-chan {:action :component-will-mount :component this}))
              (when-let [f (:component-will-mount-fn opts)] (f this))
              (let [{:keys [chan action-chan created-chan clean-chan]} (get-state this)

                    do-validation (partial do-meta-validation this)]
                (when mount-info-atom (reset! mount-info-atom {:component this :chan chan :schema-coercer schema-coercer}))
                (when-not (get-in opts [:action :no-reset]) (set-state! this :inputs initial-bs))
                (set-state! this :action-state initial-action-state)
                (do-validation)

                (go
                  (loop []
                    (when (<! clean-chan)
                      (clean app this)
                      (update-state! this [:action-state :action] action-states)
                      (update-state! this :inputs #(enable-all %))
                      (recur))))

                (go
                  (loop []
                    (when-let [[v m] (<! created-chan)]
                      (if (= :ko v)
                        (do
                          (prn (str "An error has occured during action : " m))
                          (recur))
                        (if (get-in opts [:action :one-shot])
                          (do
                            (update-state! this [:action-state :action] action-states)
                            (update-state! this [:action-state :clean] action-states)
                            (update-state! this :inputs #(disable-all %)))
                          ))
                      (recur))))

                (go
                  (loop []
                    (when (<! action-chan)
                      (let [okay (do-validation)]
                        (when okay
                          (set-state! this [:action-state :action] :active)
                          (let [v (get-state this :inputs)
                                raw (va/pre-validation v)
                                coerced (schema-coercer raw)]
                            (when-let [to-do action]
                              (let [to-send {:data           coerced
                                             :app            app
                                             :this           this
                                             :schema-coercer schema-coercer
                                             }]
                                (js/setTimeout
                                  #(dispatch to-do to-send) 10))
                              )

                            )))
                      (recur)))))))

          :component-did-mount
          (fn [this]
            (binding [*component* this]
              (when ext-chan (dispatch ext-chan {:action :component-did-mount :component this}))
              (when-let [f (:component-did-mount-fn opts)] (f this))
              (handle-date-fields! this d/default-fmt opts)))

          :component-will-unmount
          (fn [this]
            (binding [*component* this]
              (when ext-chan (dispatch ext-chan {:action :component-will-unmount :component this}))
              (when-let [f (:component-will-unmount-fn opts)] (f this))
              (close-channels this)))

          :component-will-update
          (fn [this next-props next-state]
            (binding [*component* this]
              (when ext-chan (dispatch ext-chan {:action     :component-will-update
                                                 :component  this
                                                 :next-props next-props
                                                 :next-state next-state}))
              (when-let [f (:component-will-update-fn opts)] (f this next-props next-state))))

          :render
          (fn [this]
            (binding [*component* this]
              (when ext-chan (dispatch ext-chan {:action    :render
                                                 :component this}))
              (when-let [f (:render-fn opts)] (f this))
              (let [{:keys [chan inputs action-state dyn-opts] :as state} (get-state this)]
                (let [labels (comp-i18n this comp-name schema opts)
                      title (get-in labels [:title])
                      opts (merge-with merge opts dyn-opts)
                      comp-class (get-in opts [comp-name :className])]
                  [:div {:className (styles "panel panel-default" comp-class)
                         :key       (full-name comp-name)
                         :ref       (full-name comp-name)
                         :id        (full-name comp-name)}
                   (when title
                     [:div {:className "panel-heading"}
                      [:h3 {:className "panel-title"} title]])
                   [:form {:className "panel-body"
                           :role      "form"}
                    (into [:div {:className "inputs-group"}]
                          (if order
                            (map (fn [k] (build-input this (assoc (k opts) :k k :k-sch
                                                                           (su/get-sch schema k)
                                                                           :i18n (k labels)))) order)
                            (map (fn [[k t]]
                                   (let [k (if (keyword? k) k (:k k))]
                                     (build-input this (assoc (k opts) :k k :k-sch t :i18n (k labels))))) schema)))
                    [:div {:className "panel-button"}
                     [(build-component button-view state this {:state state
                                                               :opts  {:k         :action
                                                                       :labels    (or
                                                                                    (:action labels)
                                                                                    "Submit")
                                                                       :comp-name comp-name
                                                                       :attrs     (get-in opts
                                                                                          [:action :attrs])}})]
                     #_[(build-component button-view state this {:state state
                                                                 :opts  {:k         :clean
                                                                         :labels    (or (:clean labels) "Clear")
                                                                         :comp-name comp-name
                                                                         :attrs     (get-in opts [:clean :attrs])}})]]]]))))})))))

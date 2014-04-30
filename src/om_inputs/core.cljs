(ns om-inputs.core
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [schema.macros :as s])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [chan put! >! <! alts!]]
            [cljs.core.async.impl.channels :refer [ManyToManyChannel]]
            [schema.core :as s]
            [clojure.string :as str]))

(enable-console-print!)

;;;;;;;;;; Low level Clojure Utils ;;;;;;;;;;;;;;;

(defn make-map-with-vals
  "[{:a 1 :b 2} {:a 3 :b 4}] :a :b -> {1 2, 3 4}
   [{:a 1 :b 2} {:a 3 :b nil}] :a :b -> {1 2}"
  [ms k1 k2]
  (into {} (for [m ms
                 :let [[v1 v2] ((juxt k1 k2) m)]
                 :when v2]
            [v1 v2])))

;;(make-map-with-vals [{:a 1 :b 2} {:a 3 :b nil}] :a :b)

;;;;;;;;; i18n Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->label
  "Translate labels or business data
  If a label is not found the more precise keyword is used"
  ([ref-data m k]
   "Find label in the ref data"
   (if-let [label (->> k
                    ref-data
                    (some #(when ((comp  #{(k m)} :code) %) %))
                    :label)]
     label
     (str/capitalize (name k))))
  ([ref-data ks]
   (get-in ref-data ks (str/capitalize (name (last ks))))))


;;;;;;;;;;;;;;; Debug Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn trace
  "Display the raw data."
  [app owner]
  (om/component
   (dom/div nil (pr-str app))))

(defn debug [original owner opts]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:style #js {:border "1px solid #ccc"
                                :padding "5px"}}
        (dom/div nil
          (dom/span nil "Data :")
          (dom/pre #js {:style #js {:display "inline-block"}}
            (pr-str (second original))))
               (apply om/build* original)
               (dom/span nil "---")))))


;;;;;; Generic single input ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn e-value
  "Get value from an event"
  [e]
  (-> e .-target .-value))

(defn make-select
  [c l k v data]
  (dom/span nil
           (dom/label nil l)
           (apply dom/select #js {:value v
                                  :onChange #(put! c [k (e-value %)])}
                       (dom/option #js {:value ""} "")
                       (map (fn [{:keys [code label]}]
                              (dom/option #js {:value code} label)) data))))

(defn- make-input
  "Low level function to make an input field with 2 ways linkage."
  ([c l k v ]
   (make-input c l k v {}))
  ([c l k v  opts]
   (let [put-chan #(put! c [k (e-value %)])]
     (dom/span nil
                    (dom/label nil l)
                    (dom/input (clj->js (merge {:value v
                                                :onChange put-chan} opts)))
               (when (:labeled opts)
                 (dom/label nil v))))))


;;;;;;;;;;;;;; Complete inputs form ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def sch-inputs {:inputs s/Any})

(def sch-chan {:chan ManyToManyChannel})


(def sch-i18n {:i18n {:input s/Any}})

(def sch-state (merge sch-inputs sch-chan {s/Any s/Any}))


(s/defn  ^:always-validate magic-input
        [k :- s/Keyword
         state :- sch-state
         shared :- sch-i18n
         opts]
        (let [{:keys [chan inputs]} state
              i18n (:i18n shared)]
          (make-input chan (->label i18n [:input k]) k (k inputs) opts)))

(defn build-input
  "Handle the display of an input from state and push change on a channel.
   The map of inputs is expected in state under the key :inputs
   The channel is expected in state under key :chan
   The i18n fn is expected in shared under key :i18n
   Handle the display of an input from state and push change on a queue
   The channel is expected in state under key :chan"
  ([owner k opts]
   (let [state (om/get-state owner)
         shared (om/get-shared owner)]
     (magic-input k state shared opts)))
  ([owner k]
   (build-input owner k {})))


(def input [{:code :label :value "" :coercer (fn [n o](str/upper-case n))}
            {:code :version :value "" :coercer (fn [n o] (if (re-matches #"[0-9]*" n) n o))}
            {:code :tier :value ""}
            {:code :cat :value "" :opts {:type "select"}}
            {:code :level :value 4 :coercer #(js/parseInt %) :opts {:type "range" :min 0 :max 5 :labeled true}}
            {:code :comment :value ""}])


(def sch-conf-opts {(s/optional-key :labeled) s/Bool
                    (s/optional-key :min) s/Int
                    (s/optional-key :max) s/Int
                    (s/optional-key :type) (s/enum "text" "range" "number" "color" "select") })


(def sch-conf [{:code s/Keyword
                :value s/Any
                (s/optional-key :coercer) s/Any
                (s/optional-key :opts) sch-conf-opts}])



(s/defn build-init
  "Build the init map backing the inputs in the form."
  [m :- sch-conf]
  (make-map-with-vals m :code :value))


(s/defn build-coercers
        [m :- sch-conf]
        (make-map-with-vals m :code :coercer))


(defn key-value-view
  [entry owner]
  (om/component
   (dom/label #js {:className "item-view"} (val entry))))


(defn item-view
  [item owner]
  (om/component
   (apply dom/span nil (om/build-all key-value-view item))))


(s/defn ^:always-validate make-input-comp
  "Build an input form Om component based on the config"
  [conf :- sch-conf]
  (fn [app owner]
    (reify
      om/IInitState
      (init-state [_]
                  {:chan (chan)
                   :inputs (build-init conf)
                   :coercers (build-coercers conf)})
      om/IWillMount
      (will-mount [this]
                  (let [{:keys [coercers chan inputs] :as state} (om/get-state owner)]
                    (go
                     (loop []
                       (let [[k v] (<! chan)
                             coerce (get coercers k (fn [n _] n))
                             o (om/get-state owner [:inputs k])]
                         (condp = k
                           :create (do
                                     (om/transact! app #(conj % v))
                                     (om/set-state! owner [:inputs] inputs))
                           (om/set-state! owner [:inputs k] (coerce v o))))
                       (recur)))))
      om/IRenderState
      (render-state [_ {:keys [chan inputs] :as state}]
                    (let [i18n (om/get-shared owner :i18n)]
                      (dom/fieldset #js {:className "form"}
                                    (into-array (map (fn [{:keys [code opts]}]
                                                      (build-input owner code opts)) conf))
                                    (dom/input #js {:type  "button"
                                                    :value (->label i18n [:input :create])
                                                    :onClick #(put! chan [:create inputs])})
                                    (apply dom/div nil (om/build-all item-view app))))))))




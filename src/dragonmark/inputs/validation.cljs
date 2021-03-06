(ns dragonmark.inputs.validation
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [dragonmark.inputs.extern :refer [get-state set-state! update-state!
                             get-node create-component get-i18n-info
                             build-component]]
   [clojure.set :as st]
   [clojure.string :as str]
   [jkkramer.verily :as v :refer [validation->fn]]
   [schema.core :as s :include-macros true]
   [schema.coerce :as coerce]
   [dragonmark.inputs.schemas :as su :refer [ sch-field sch-business-state sch-field-state]]
   [dragonmark.inputs.date-utils :as d]))






;___________________________________________________________
;                                                           |
;          Errors Schemas                                   |
;___________________________________________________________|


(def sch-errors-list
  "The validation error message keys for a field.
   The key is not the rule key but the message key."
  [(s/named s/Keyword "message")])

(def sch-errors
  "Describes the input's error data structure.
   A field can have multiples errors."
  {sch-field sch-errors-list})


(def sch-verily-errs
  "Describes the Verily errors data structure."
  [{:keys [sch-field]
    :msg s/Keyword}])

(def sch-schema-errs
  "Describes the Schema errors data structure"
  {sch-field s/Any})



;___________________________________________________________
;                                                           |
;          Coercers                                         |
;___________________________________________________________|


(defn empty-string-coercer
  "Do not validate an empty string as a valid s/Str"
  [s]
  (if (str/blank? s) nil s))

(defn inst-coercer
  "Coerce an input string to a date.
  The default format is used.
  A blank string is coerced to nil."
  [s]
  (when-not (str/blank? s)
    (d/parse (d/fmt d/default-fmt s))))



(def validation-coercer
  "Schema coercers used for inputs that are sent as string."
  {s/Num (coerce/safe coerce/edn-read-string)
   s/Regex empty-string-coercer
   s/Str empty-string-coercer
   (s/maybe s/Str) empty-string-coercer
   s/Inst inst-coercer})

;; ((coerce/coercer {:size s/Num} validation-coercer) {:size 89})


;___________________________________________________________
;                                                           |
;          Validation handlers                              |
;___________________________________________________________|


(defn validate
  "Generic sequence of validation.
  The first args can be partially applied to generate a custom validator."
  [validation-fn post m]
  (post (validation-fn m)))


(s/defn transform-schema-errors :- sch-errors
  "Transforms the Schema's error data structure into the common error data structure.
  For the moment Schema error are treated as missing field."
  [errs :- sch-schema-errs]
  (when-let [errors (:error errs)]
   (apply merge-with concat
         (for [[k _] errors]
           {k [:mandatory]}))))


;___________________________________________________________
;                                                           |
;          Verily Validation handlers                        |
;___________________________________________________________|


(s/defn transform-verily-errors :- sch-errors
  "Transforms the Verily's error data structure into the common error data structure."
  [errs :- sch-verily-errs]
  (when (seq errs)
    (apply merge-with concat {}
         (for [{:keys [keys msg]} errs
               k keys]
           {k [msg]}))))


(defn build-verily-validator
  [rules]
  (partial validate (v/validations->fn rules) transform-verily-errors ))



(def inter-fields-rules
  #{:equal
    :greater
    :lower
    :greater-or-equal
    :lower-or-equal})

(defn error->rule
  "Build the map error->rule"
  [rules]
  (into {} (for [ r rules]
    ((juxt last first) r))))



(defn fields-dependencies
  "Determines validation dependencies between fields"
  ([rules]
   (fields-dependencies inter-fields-rules rules))
  ([rule-keys rules]
   (into {}
         (for [[r fs m] rules
               :when (rule-keys r)
               :let [[f & deps] fs]]
           {f {:deps deps
               :mess m
               :rule r}}))))


;___________________________________________________________
;                                                           |
;          Complete Validation                              |
;___________________________________________________________|


(s/defn handle-errors :- sch-business-state
  "Set valid to false for each key in errors, true if absent"
  [state :- sch-business-state
   errs :- sch-errors]
  (let [err-ks (set (keys errs))
        all-ks (set (keys state))
        valid-ks (st/difference all-ks err-ks)
        state (reduce (fn [s e]
                        (-> s
                            (assoc-in [e :valid] false)
                            (assoc-in [e :error] (e errs)))) state err-ks)]
    (reduce (fn [s e]
              (-> s
               (assoc-in [e :valid] true)
               (update-in [e] dissoc :error))) state valid-ks)))


(s/defn keep-fields-to-validate :- sch-business-state
  [bs :- sch-business-state]
  (into {} (for [[k m] bs
                 :let [in (:value m)
                       req (:required m)]
                 :when (or req (not (str/blank? in)))]
             {k m} )))



(s/defn bs->unit-map :- {s/Keyword s/Any}
  "Extract map fk->value for a single field."
  [bs :- sch-business-state
   fk :- s/Keyword]
  (let [m (fk bs)]
    {fk (:value m)}))


(s/defn business-state->map :- {s/Keyword s/Any}
  "Transform the business local state into final map"
  [bs :- sch-business-state]
  (into {} (for [[k _] bs]
              (bs->unit-map bs k))))


(s/defn pre-validation :- {s/Keyword s/Any}
  "Create the map that will be validated by the Schema :
  Only keeps :
  - required keys
  - optional keys with non blank values"
  [bs :- sch-business-state]
  (-> bs
      keep-fields-to-validate
      business-state->map))


(s/defn sch-glo->unit :- {s/Keyword s/Any}
  "Transform a Schema into a map of key -> individual Schema"
  [sch ]
  (into {} (for [ [k t] sch]
    {(get k :k k) {k t}})))


(defn build-unit-coercers
  "Build the map of field -> coercer"
  [sch]
  (apply merge (for [[k s] (sch-glo->unit sch)]
                 {k (coerce/coercer s validation-coercer)})))

(s/defn ^:always-validate
  unit-schema-validators :- {s/Keyword s/Any}
  [unit-coercers :- {s/Keyword s/Any}]
  (apply merge (for [[k c] unit-coercers]
                 {k (partial validate c transform-schema-errors)})))

;___________________________________________________________
;                                                           |
;          Unit Validation                                  |
;___________________________________________________________|


(s/defn validate? :- s/Bool
  "Indicates if a field must be validated :
  - required field
  - optional field with non blank values"
  [s :- sch-field-state]
  (let [{:keys [required value]} s]
   (or required
      (not (str/blank? value)))))


(s/defn add-field-error :- sch-business-state
  "Handle errors for a single field"
  [state :- sch-business-state
   errs :- sch-errors]
  (reduce (fn [s e]
            (-> s
                (assoc-in [e :valid] false)
                (assoc-in [e :error] (e errs)))) state (keys errs)))

(defn no-async-error?
  [state k]
  (let [se (-> state k :async-error)]
    (boolean
      (or (nil? se)
          (empty? se)))))

(s/defn remove-field-error :- sch-business-state
  [state :- sch-business-state
   k :- s/Keyword]
  (-> state
      (assoc-in [k :valid] (no-async-error? state k))
      (update-in [k] dissoc :error)))

(s/defn remove-field-error-and-server :- sch-business-state
  [state :- sch-business-state
   k :- s/Keyword]
  (-> state
      (assoc-in [k :valid] true)
      (update-in [k] dissoc :async-error)
      (update-in [k] dissoc :error)))



(s/defn remove-dependant-errors :- [s/Keyword]
  "Remove the cross field errors keys from the validation errors.
  This prevent errors from showing when inline validation occurs."
  [cross-errs :- #{s/Keyword}
   error-rule :- {s/Keyword s/Keyword}
   errs :- [s/Keyword]]
  (seq (remove
        (fn [err] (cross-errs (get error-rule err))) errs)))



(defn build-error-remover [rules cross-fields-rules]
  (partial remove-dependant-errors cross-fields-rules  (error->rule rules)))

(defn unit-verily-validation
  "validate a single field against verily rules.
   If an other field depends on this one, then the errors linked to this validation won't show up."
  [fk unit {:keys [unit-coercers  remove-errs-fn verily-validator] :as state}]
  (let [coerced ((fk unit-coercers) unit)
        errs (verily-validator coerced)]
    (when (contains? errs fk)
      (let[errs-unit (update-in errs [fk] remove-errs-fn)
           res (select-keys errs-unit [fk])]
          (when (fk res)
            res)))))


(defn unit-dependant-verily-validation
  "Verily validation of a field that depend on other.
   The confirm password is a typical example."
  [fk inputs state]
  (let [{:keys [validation-deps verily-validator]} state
         deps (fk validation-deps)
        ;coerced ((fk unit-coercers) bs)
        coerced (business-state->map inputs)
        errs (verily-validator coerced)]
    (when (contains? errs fk)
     (select-keys errs [fk]))))


(defn verily-validation
  "Verily validation of a single field"
  [fk unit bs state]
  (let [{:keys [validation-deps]} state]
    (if (fk validation-deps)
      (unit-dependant-verily-validation fk bs state)
      (unit-verily-validation fk unit state))))



(defn field-validation
  "Validation of a single field
   TODO Refactoring"
  [fk inputs state]
  (let [{:keys [unit-validators]} state
        unit (bs->unit-map inputs fk)]
    (if (validate? (fk inputs))
      (if-let [errs (or (and fk (if-let [the-fn (fk unit-validators)]
                                  (the-fn unit)))
                        (verily-validation fk unit inputs state))]
        (add-field-error inputs errs)
        (remove-field-error inputs fk))
      (remove-field-error inputs fk))))

(declare update-action!)

(defn field-validation!
  "Validate a single field of the local business state and update the local state."
  ([owner f]
   (let [{:keys [inputs] :as state} (get-state owner)
         old-inputs inputs
         inputs (remove-field-error-and-server inputs f)]
     (let [new-business-state (field-validation f inputs state)]
       (when (not= old-inputs new-business-state)
         (set-state! owner [:inputs] new-business-state)
         (update-action! owner)
         )))))


(defn full-validation
  [inputs state]
  (reduce  (fn [bs f] (field-validation f bs state)) inputs (keys inputs)))


(s/defn no-error? :- s/Bool
  "Tells if there is at least one field in error."
  [bs :- sch-business-state]
  (not-any? (fn [[k v]]
              (false? (:valid v))) bs))

(defn update-action!
  [this]
  (let [{:keys [inputs] :as state} (get-state this)
        no-error (no-error? inputs)]
    (if no-error
      (set-state! this [:action-state :action] nil)
      (set-state! this [:action-state :action] :in-error))
    no-error)
  )
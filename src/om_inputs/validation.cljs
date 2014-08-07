(ns om-inputs.validation
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [schema.macros :as s])
  (:require [om.core :as om :include-macros true]
            [clojure.set :as st]
            [clojure.string :as str]
            [jkkramer.verily :as v]
            [schema.core :as s]
            [schema.coerce :as coerce]
            [om-inputs.schemas :as su :refer [ sch-field sch-business-state sch-field-state]]
            [om-inputs.date-utils :as d]))


;___________________________________________________________
;                                                           |
;          Errors Schemas                                   |
;___________________________________________________________|


(def sch-errors-list
  "The validation error message keys for a field.
   The key is not the rule key but the message key."
  [(s/named s/Keyword "message")])

(def sch-errors
  "Discribes the om-input's error data structure.
   A field can have multiples errors."
  {sch-field sch-errors-list})


(def sch-verily-errs
  "Describe the Verily errors data structure."
  [{:keys [sch-field]
    :msg s/Keyword}])

(def sch-schema-errs
  "Describe the Schema errors data structure"
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
  [s]
  (when-not (str/blank? s)
    (d/parse (d/fmt d/default-fmt s))))


(def validation-coercer
  (merge coerce/+string-coercions+ {s/Str empty-string-coercer
                                    (s/maybe s/Str) empty-string-coercer
                                    s/Inst inst-coercer}))


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


(defn verily
  "Inversion of parameters for the verily validation function for partial application of the rules."
  [validators m]
  (v/validate m validators))


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
  #{:equal})

(defn error->rule
  "construit la map error->rule"
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


(s/defn ^:always-validate  handle-errors :- sch-business-state
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



(s/defn ^:always-validate bs->unit-map :- {s/Keyword s/Any}
  "Extract map fk->value for a single field."
  [bs :- sch-business-state
   fk :- s/Keyword]
  (let [m (fk bs)]
    {fk (:value m)}))


(s/defn ^:always-validate business-state->map :- {s/Keyword s/Any}
  "Transform the business local state into final map"
  [bs :- sch-business-state]
  (into {}(for [[k m] bs]
              (bs->unit-map bs k))))


(s/defn ^:always-validate
  pre-validation :- {s/Keyword s/Any}
  "Create the map that will be validated by the Schema :
  Only keeps :
  - required keys
  - optional keys with non blank values"
  [bs :- sch-business-state]
  (-> bs
      keep-fields-to-validate
      business-state->map))


(s/defn ^:always-validate
  sch-glo->unit :- {s/Keyword s/Any}
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


(s/defn ^:always-validate
  validate? :- s/Bool
  "Indicates if a field must be validated :
  - required field
  - optional field with non blank values"
  [s :- sch-field-state]
  (let [{:keys [required value]} s]
   (or required
      (not (str/blank? value)))))


(s/defn ^:always-validate
  add-field-error :- sch-business-state
  "Handle errors for a single field"
  [state :- sch-business-state
   errs :- sch-errors]
  (reduce (fn [s e]
            (-> s
                (assoc-in [e :valid] false)
                (assoc-in [e :error] (e errs)))) state (keys errs)))

(s/defn ^:always-validate
  remove-field-error :- sch-business-state
  [state :- sch-business-state
   k :- s/Keyword]
  (-> state
      (assoc-in [k :valid] true)
      (update-in [k] dissoc :error)))



(s/defn ^:always-validate remove-dependant-errors :- [s/Keyword]
  "Remove the cross field errors keys from the validation errors.
  This prevent errors from showing when inline validation occurs."
  [cross-errs :- #{s/Keyword}
   error-rule :- {s/Keyword s/Keyword}
   errs :- [s/Keyword]]
  (seq (remove
        (fn [err] (cross-errs (get error-rule err))) errs)))


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
  [fk state]
  (let [{:keys [inputs validation-deps unit-coercers verily-validator]} state
         deps (fk validation-deps)
        ;coerced ((fk unit-coercers) bs)
        coerced (business-state->map inputs)
        errs (verily-validator coerced)]
    (when (contains? errs fk)
     (select-keys errs [fk]))))


(defn verily-validation
  [fk unit state]
  (let [{:keys [validation-deps]} state]
    (if (fk validation-deps)
      (unit-dependant-verily-validation fk state)
      (unit-verily-validation fk unit state))))



(defn field-validation!
  "Validate a single field of the local business state"
  [owner f ]
  (let [business-state (om/get-state owner :inputs)
        {:keys [unit-validators unit-coercers verily-validator] :as state} (om/get-state owner)
        field-state (f business-state)
        unit {f (:value field-state)}]
    (when (validate? field-state)
      (om/set-state! owner [:inputs]
                     (if-let [errs (or ((f unit-validators) unit)
                                       (verily-validation f unit state) )]
                       (add-field-error business-state errs)
                       (remove-field-error business-state f))))))



#_(

     ;; Processus de validation
     ;; Faut il identifier les validation inter champs afin de les jouer au bon moment ?
     ;; Une piste : Les erreurs verily nous indique si la validation est inter champs
     ;; The source schema
    (def dummy-sch {:email s/Str
                    :confirm-email s/Str
                    (s/optional-key :size) s/Int
                    (s/optional-key :company) s/Str})

   ;; The units schemas

   (sch-glo->unit dummy-sch)


   ;; business-state input

   (def bs {:email {:value "email"
                    :required true
                    :type s/Str}
            :confirm-email {:value "titi"
                            :type s/Str
                            :required true}
            :size {:value "7"
                   :required false
                   :type s/Int}
            :company {:value ""
                      :required false
                      :type s/Str}})

   ;; pre-validation keep required fields or non required with value.

   (pre-validation bs)

   (def pvbs {:email "email"
              :confirm-email "titi"
              :size "7"})

   ;; Coercion
   (def sch-coercer (coerce/coercer dummy-sch validation-coercer))

   (def pvcbs (sch-coercer pvbs))

    (assert (= pvcbs {:email "email"
               :confirm-email "titi"
               :size 7}))

   ;;Validation ->
   (def rules [[:min-val 10 :size :size-min-length]
               [:email [:email :confirm-email] :bad-email]
               [:equal [:email :confirm-email] :email-match]])

   (def validator (build-verily-validator rules))

   (validator pvcbs)

   (validator {:email "kjk"})

   (filter #(vector? (:keys %)  ) ((v/validations->fn rules) pvcbs))


   (unit-verily-validation :email {:email "hkhjk"} (build-unit-coercers  dummy-sch) validator)


    ((:email (unit-schema-validators dummy-sch)) {:email nil})

   )
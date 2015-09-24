(ns dragonmark.inputs.extern
  (:require [reagent.core :as reagent :refer [atom]]))

;_________________________________________________
;                                                 |
;          Platform specific multi-methods        |
;_________________________________________________|

(defmulti get-state (fn [_] true))
(defmulti set-state! (fn [_] true))
(defmulti update-state! (fn [_] true))
(defmulti get-node (fn [_] true))
(defmulti create-component (fn [_] true))
(defmulti get-i18n-info (fn [_] true))
(defmulti build-component (fn [_] true))

(defonce error-messages (atom {:errors {:mandatory "This field is mandatory"
                                        :bad_email "Invalid Email Address"
                                        :email_match "Email Addresses must match"}}))

(defonce before-get-i18 (atom nil))
(defonce after-get-i18 (atom nil))

(defmethod get-i18n-info true
  [& x]
  (when-let [f @before-get-i18] (apply f x))
  (let [ret
        @error-messages]
    (if-let [f @after-get-i18]
      (f ret)
      ret)))

(defonce before-create-component (atom nil))
(defonce after-create-component (atom nil))

(defmethod create-component true
  [& x]
  (when-let [f @before-create-component] (apply f x))
  (let [ret
        (apply reagent/create-class x)]
    (if-let [f @after-create-component]
      (f ret)
      ret)
    ))


(defmethod get-node true
  [x & [y & z]]
  (let [ret
        (reagent/dom-node x)]
    (if y
      (.querySelector ret (str "#" (name  y)))
      ret)
    ))

(defonce before-get-state (atom nil))
(defonce after-get-state (atom nil))


(defmethod get-state true
  [a & [rst]]
  (when-let [f @before-get-state] (f a rst))
  (let [ret
        (let [atm
              (cond
                (satisfies? IDeref a)
                a

                a
                (reagent/state-atom a)

                :else
                (atom {}))
              x (deref atm)

              rst (cond
                    (nil? rst) nil
                    (sequential? rst) rst
                    :else [rst])

              reduction (reduce #(%2 %1) x rst)]
          reduction)]
    (if-let [f @after-get-state]
      (f ret)
      ret)))

(defonce before-update-state (atom nil))
(defonce after-update-state (atom nil))

(defmethod update-state! true
  [owner keys func]
  (when-let [f @before-update-state] (f owner keys func))
  (let [ret (swap! (reagent/state-atom owner) update-in keys func)]
    (if-let [f @after-update-state]
      (f ret)
      ret)))

(defonce before-set-state (atom nil))
(defonce after-set-state (atom nil))

(defmethod set-state! true
  [a b c]
  (when-let [f @before-set-state] (f a b c))
  (let [ret
        (let [the-fn (cond
                       (keyword? b)
                       #(assoc % b c)
                       (vector? b)
                       #(assoc-in % b c)
                       :else identity)]
          (cond
            (nil? a)
            {}

            (satisfies? IDeref a)

            (swap! a the-fn)

            :else
            (swap! (reagent/state-atom a) the-fn)))]
    (if-let [f @after-set-state]
      (f ret)
      ret)))

(defonce before-build-component (atom nil))
(defonce after-build-component (atom nil))

(defmethod build-component true
  [the-fn app owner {:keys [state opts] :as o2} & rst]

  (when-let [f @before-build-component] (apply f o2 rst))
  (let [ret
        (let [the-comp (the-fn app owner (or opts o2))]
          (when (and false state)
            (swap! (reagent/state-atom the-comp) merge state))
          the-comp)]
    (if-let [f @after-build-component]
      (f ret)
      ret)))

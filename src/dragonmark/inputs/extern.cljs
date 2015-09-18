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

(defmethod get-i18n-info true
  [& x]
  {:errors {:mandatory "This field is mandatory"
            :bad_email "Invalid Email Address"
            :email_match "Email Addresses must match"}})


(defmethod create-component true
  [& x]
  (apply reagent/create-class x))

(defmethod get-node true
  [x & [y & z]]
  (let [ret
        (reagent/dom-node x)]
    (if y
      (.querySelector ret (str "#" (name  y)))
      ret)
    ))



(defmethod get-state true
  [a & [rst]]
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
    reduction))

(defmethod update-state! true
  [owner keys func]

  (swap! (reagent/state-atom owner) update-in keys func)
  )


(defmethod set-state! true
  [a b c]
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
      (swap!  (reagent/state-atom a) the-fn))))

(defmethod build-component true
  [the-fn app owner {:keys [state opts] :as o2} & rst]


  (let [the-comp (the-fn app owner (or opts o2))]
    (when (and false  state)
      (swap! (reagent/state-atom the-comp) merge state))
    the-comp))

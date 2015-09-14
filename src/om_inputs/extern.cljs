(ns om-inputs.extern)

                                        ;_________________________________________________
                                        ;                                                 |
                                        ;          Platform specific multi-methods        |
                                        ;_________________________________________________|

(defmulti get-state (fn [_] true))
(defmulti set-state! (fn [_] true))
(defmulti update-state! (fn [_] true))
(defmulti update-state-nr! (fn [_] true))
(defmulti get-node (fn [_] true))
(defmulti create-component (fn [_] true))
(defmulti get-i18n-info (fn [_] true))
(defmulti build-component (fn [_] true))

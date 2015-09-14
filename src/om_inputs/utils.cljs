(ns om-inputs.utils
  (:require
   [clojure.string :as str]
   [om-inputs.extern :refer [get-state set-state! update-state! update-state-nr!
                             get-node create-component get-i18n-info
                             build-component]]
            ))



;_________________________________________________
;                                                 |
;          Interop Utils                          |
;_________________________________________________|


(defn ->int [s]
  (when-not (str/blank? s) (js/parseInt s 10)))


;_________________________________________________
;                                                 |
;          Clojure Utils                          |
;_________________________________________________|


(defn full-name
  "Get the namespace of a keyword"
  [k]
  (if (namespace k)
   (str/join "/" ((juxt namespace name) k))
   (name k)))




;_________________________________________________
;                                                 |
;          Display Utils                          |
;_________________________________________________|


(defn ^:private set-comp-class!*
  "Permet modifier dynamiquement la classe du div racine d'un composant."
  [render? owner cpt style]
  (let [args [owner [:dyn-opts cpt :className] style]]
    (if render?
      (apply set-state! args)
      (apply set-state-nr! args))))

(def set-comp-class!
  (partial set-comp-class!* true))

(def set-comp-class-nr!
  (partial set-comp-class!* false))

(ns dragonmark.inputs.view
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [reagent.core :as reagent :refer [atom]]
   [cljs.core.async :refer [chan timeout put! >! <! alts! close!]]
   [dragonmark.inputs.core :as in :refer [build-input make-input-comp]]
   [clojure.string :as str]
   [schema.core :as s]
   [dragonmark.inputs.date-utils :refer [at tomorrow]]
   [goog.net.XhrIo :as xhr]
   [cljs.reader :as reader]
   [goog.events :as events]

   [figwheel.client :as fw :include-macros true]
   [dragonmark.inputs.validation :as va]
   [dragonmark.inputs.extern :refer [get-i18n-info get-state build-component get-state]]

   )
  (:import
           goog.net.EventType
           [goog.events EventType]
           [goog.ui IdGenerator]))

;; decomment (ws-repl/connect "ws://localhost:9001")


(defmethod get-i18n-info true
  [& x]
  {:errors {:mandatory "This field is mandatory"
            :bad_email "Invalid Email Address"
            :email_match "Email Addresses must match"}})


(defn display-edn [edn]
  (js/alert edn))

(def mounted (atom nil))

(def input-view (make-input-comp
                  :create-person
                  {:person_first_name                   (s/maybe s/Str)
                   :person_date_aller                   s/Inst
                   (s/optional-key :person_date_retour) s/Inst
                   :person_name                         s/Str
                   :person_email                        s/Str
                   :person_email_confirm                s/Str
                   :person_password                     s/Str
                   :person_birthdate                    s/Inst
                   :person_size                         (s/named s/Num "size")
                   :person_age                          (s/named s/Int "age")
                   :person_gender                       (s/enum "M" "F")
                   :person_married                      (s/eq true)}

                  (fn [{:keys [data]}]
                    (.log js/console "Data " (pr-str data))
                    (let [v (rand-int 10)]
                      (.log js/console "V is " v)
                      (if (> 2 v)
                        (set! (.-location js/window) "/")
                        (some-> mounted deref :chan (in/dispatch [:update-input
                                                                  [
                                                                   [:add-error :person_name (str "yak "v)]]]))))
                    (.log js/console "Yo"))
                  {:create-person             {:className "visible"}
                   :validate-i18n-keys        false

                   :mount-info-atom           mounted
                   :component-will-unmount-fn (fn [x]
                                                (.log js/console "Unmounted data "
                                                      (when-let [sc (some-> mounted deref :schema-coercer)]
                                                        (pr-str (sc x)))))

                   :init                      {
                                               :person_gender        "F"
                                               :person_size          187.50
                                               :person_birthdate     (tomorrow)
                                               :person_email         "h@h"
                                               :person_email_confirm "d@h"
                                               :person_married       true
                                               :person_name          "MADELAINE"}
                   :order                     [:person_date_aller :person_date_retour
                                               :person_first_name :person_name
                                               :person_email
                                               :person_email_confirm
                                               :person_password
                                               :person_gender
                                               :person_birthdate :person_age
                                               :person_size :person_married]
                   :person_birthdate          {:label "Birthday"}
                   :person_first_name         {:layout "horizontal"
                                               :label  "First Name"
                                               :attrs  {:tabIndex 0}}
                   :person_gender             {;:type "btn-group"
                                               :label       "Sex"
                                               :label-order true}
                   :person_password           {:label "Password" :type "password"}
                   :person_name               {:label "Last Name"}
                   :person_email              {:type  "email"
                                               :label "Email"
                                               :attrs {:tabIndex 0}}
                   :person_email_confirm      {:type  "email"
                                               :label "Email - Confirm"}
                   :person_date_aller         {:type    "now"
                                               :label   "Leaving"
                                               :labeled true
                                               :attrs   {:tabIndex 0}}
                   :mandatory                 {:lable "Mandatory Field"}
                   :person_size               {:label "Height"}
                   :person_date_retour        {
                                               :label "Return"}
                   ;:person_size {:attrs {:type "number"}}
                   :person_age                {:type  "stepper"
                                               :label "Age"
                                               :attrs {:min "0" :max "10" :step 2 :size "lg"}}
                   :person_married            {:layout "in-line"
                                               :label  "Married"}
                   :validations               [[:positive [:person_age] :positive]
                                               ;; [:can_drink [:person_age] "No under age drinking"]
                                               [:after (at 0) :person_date_aller :date_aller]
                                               [:min-length 6 :person_password]
                                               [:email [:person_email_confirm :person_email] :bad_email]
                                               [:equal [:person_email_confirm :person_email] :email_match]]}))

(defn app
  []
  [:div {:className "container"}
   [:div "Some excellent input stuff"]
   [:div [:button
          {:onClick (fn []
                      (some-> mounted deref :chan
                              (in/dispatch
                                [:update-input [[:add-error :person_name "Yak Yak3" ]]])
                              )
                      nil
                      )}
          "Insert async stuff"]]
   [(build-component input-view (:client app) nil nil)]]
  )



(defn init! []
  (reagent/render [app] (. js/document (getElementById "person"))))

(enable-console-print!)

(fw/watch-and-reload
 :websocket-url "ws://localhost:3449/figwheel-ws"
 :jsload-callback init!)

(init!)

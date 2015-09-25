(defproject dragonmark/inputs "0.4.4"
  :description "Generate Web Input Form for React.js (Om or Reagent), validation included."
  :url "https://github.com/dragonmark/inputs"

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.48"  :scope "provided"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]

                 [prismatic/schema "1.0.0"]
                 [jkkramer/verily "0.6.0"]
                 [sablono "0.2.22"]

                 [cljsjs/react "0.13.3-1"]
                 [reagent "0.5.0"]

                 ;; [clj-vat "0.1.2" :scope "provided"]
                 ]

  :plugins [[lein-cljsbuild "1.0.5"]
            [lein-figwheel "0.3.3"]
            [codox "0.8.10"]]

  :min-lein-version "2.5.0"

  :uberjar-name "dragonmarkinputs.jar"

  :jvm-opts ["-Xmx1g" "-server"]

  :codox {:language :clojurescript
          :include [dragonmark.inputs.date-utils dragonmark.inputs.core]}


  :cljsbuild {
              :builds [{:id "dev"
                        :resource-paths ["examples"]
                        :source-paths ["src"  "examples/contact/src"]
                        :compiler {:output-to "examples/contact/out/inputs.js"
                                   :output-dir "examples/contact/out"
                                   :optimizations :none
                                   :source-map true}}
                       {:id "simple"
                        :resource-paths ["examples"]
                        :source-paths ["src"  "examples/contact/src"]
                        :compiler {:output-to "examples/contact/out/main.js"
                                   :optimizations :simple
                                   :pretty-print true
                                   :preamble ["react/react.min.js"]
                                   :externs ["react/externs/react.js"]}}
                       {:id "release"
                        :source-paths ["src" ]
                        :compiler {:output-to "examples/contact/out/main.js"
                                   :optimizations :advanced
                                   ;:closure-warnings {:check-useless-code :on}
                                   :pretty-print false
                                   :pseudo-names false
                                   :preamble []
                                   :externs []}}]}


  :figwheel {:http-server-root "contact"
             :server-port 3449
             :css-dirs ["examples/contact/css"]})

(defproject universe-2d "0.1.0-SNAPSHOT"
  :description "2D universe generation"
  :url "https://gitlab.com/bas080/lasius"
  :license {:name "GNU GPLv3"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 [aysylu/loom "0.6.0"]
                 [quil "2.5.0"]]
  :main ^:skip-aot universe-2d.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

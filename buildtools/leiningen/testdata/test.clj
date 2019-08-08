;; This is Leiningen's own project configuration. See doc/TUTORIAL.md
;; file as well as sample.project.clj for help writing your own.

(defproject leiningen "2.9.1"
  :description "Automate Clojure projects without setting your hair on fire."
  :url "https://github.com/technomancy/leiningen"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  ;; If you update these, update resources/leiningen/bootclasspath-deps.clj too
  :dependencies [[one "1.0.0" :exclusions [org.clojure/tools.analyzer.jvm]]

                 ;; needed for pom
                 [organization/two "2.0.0"]
                 ;; needed for test
                 [three "3.0.0"
                  :exclusions [org.clojure/clojure]]
                 ;; needed for new
  :pedantic? :abort
  ;; checkout-deps don't work with :eval-in :leiningen
  :profiles {:dev {:resource-paths ["leiningen-core/dev-resources"]
                   :test-paths ["leiningen-core/test"]}
             :uberjar {:aot [#"leiningen"
                             leiningen.core.ssl ; lazy-loaded
                             cemerick.pomegranate
                             classlojure.core
                             nrepl.core]}}
  :test-selectors {:default (complement :disabled)
                   :offline (comp (partial not-any? identity)
                                  (juxt :online :disabled))}
  :source-paths ["leiningen-core/src" "src"]
  :eval-in :leiningen)

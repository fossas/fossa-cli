;; This is Leiningen's own project configuration. See doc/TUTORIAL.md
;; file as well as sample.project.clj for help writing your own.

(defproject leiningen "2.9.1"
  :description "Automate Clojure projects without setting your hair on fire."
  :url "https://github.com/technomancy/leiningen"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  ;; If you update these, update resources/leiningen/bootclasspath-deps.clj too
  :dependencies [[leiningen-core "2.9.1"]
                 ;; needed for pom
                 [org.clojure/data.xml "0.2.0-alpha5"]
                 ;; needed for test
                 [timofreiberg/bultitude "0.3.0"
                  :exclusions [org.clojure/clojure]]
                 ;; needed for new
                 [stencil "0.5.0" :exclusions [org.clojure/core.cache]]
                 ;; needed for uberjar
                 [commons-lang "2.6"]
                 ;; needed for repl
                 [nrepl "0.6.0"]
                 ;; needed for change
                 [net.cgrand/sjacket "0.1.1" :exclusions [org.clojure/clojure]]
                 ;; bump versions of various common transitive deps
                 [net.cgrand/parsley "0.9.3" :exclusions [org.clojure/clojure]]
                 [scout "0.1.1"]
                 [commons-io "2.6"]]
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

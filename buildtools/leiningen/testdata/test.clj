;; The project is named "sample", and its group-id is "org.example".
(defproject org.example/sample "1.0.0-SNAPSHOT" ; version "1.0.0-SNAPSHOT"
  ;; Beyond this point you may prepend a form with unquote, or ~, to eval it.

;;; Project Metadata
  ;; The description text is searchable from repositories like Clojars.
  :description "A sample project"
  :url "http://example.org/sample-clojure-project"
  ;; The mailing list of the project. If the project has multiple mailing
  ;; lists, use the :mailing-lists key (bound to a seq of mailing list
  ;; descriptions as below).
  :mailing-list {:name "sample mailing list"
                 :archive "http://example.org/sample-mailing-list-archives"
                 :other-archives ["http://example.org/sample-list-archive2"
                                  "http://example.org/sample-list-archive3"]
                 :post "list@example.org"
                 :subscribe "list-subscribe@example.org"
                 :unsubscribe "list-unsubscribe@example.org"}
  ;; The project's license. :distribution should be :repo or :manual;
  ;; :repo means it is OK for public repositories to host this project's
  ;; artifacts. A seq of :licenses is also supported.
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  ;; Warns users of earlier versions of Leiningen. Set this if your project
  ;; relies on features only found in newer Leiningen versions.
  :min-lein-version "2.0.0"

  ;; You can require a specific version of Leiningen. In case of mismatch
  ;; execution will abort. When versions are compared, suffixes such as
  ;; "-SNAPSHOT" are dropped.
  ;; :exact-lein-version "2.8.2"

;;; Dependencies, Plugins, and Repositories
  ;; Dependencies are listed as [group-id/name version]; in addition
  ;; to keywords supported by Pomegranate, you can use :native-prefix
  ;; to specify a prefix. This prefix is used to extract natives in
  ;; jars that don't adhere to the default "<os>/<arch>/" layout that
  ;; Leiningen expects.
  ;; You can also strings like ["group-id/name" version] for instances
  ;; where the dependency name isn't a valid symbol literal
  :dependencies [[one "1.0.0" :exclusions [org.clojure/tools.analyzer.jvm]]

                 ;; exclusions can start on another line.
                 [three "3.0.0"
                  :exclusions [org.clojure/clojure]]
                 ;; exclusions can span multiple lines. Check brackets for this.
                 [four "4.0.0" :exclusions [[org.clojure/clojure]
                                            [org.clojure/tools.reader]
                                            [crypto-random]
                                            [crypto-equality]]]
                 [org.site.com/five "5.0.0"
                  :classifier "natives-osx"
                  ;; LWJGL stores natives in the root of the jar; this
                  ;; :native-prefix will extract them.
                  :native-prefix ""]
                 ;; quoted dependency name isn't a valid symbol literal.
                 ["organization/two" "2.0.0"]
                 ]

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

  ;; These repositories will be searched for :dependencies and
  ;; :plugins and will also be available to deploy to.
  ;; Add ^:replace (:repositories ^:replace [...]) to only use repositories you
  ;; list below.
  :repositories [["java.net" "https://download.java.net/maven/2"]
                 ["sonatype" {:url "https://oss.sonatype.org/content/repositories/releases"
                              ;; If a repository contains releases only setting
                              ;; :snapshots to false will speed up dependencies.
                              :snapshots false
                              ;; Disable signing releases deployed to this repo.
                              ;; (Not recommended.)
                              :sign-releases false
                              ;; You can also set the policies for how to handle
                              ;; :checksum failures to :fail, :warn, or :ignore.
                              :checksum :fail
                              ;; How often should this repository be checked for
                              ;; snapshot updates? (:daily, :always, or :never)
                              :update :always
                              ;; You can also apply them to releases only:
                              :releases {:checksum :fail :update :always}}]
                 ;; Repositories named "snapshots" and "releases" automatically
                 ;; have their :snapshots and :releases disabled as appropriate.
                 ;; Credentials for repositories should *not* be stored
                 ;; in project.clj but in ~/.lein/credentials.clj.gpg instead,
                 ;; see `lein help deploying` under "Authentication".
                 ["snapshots" "https://blueant.com/archiva/snapshots"]
                 ["releases" {:url "https://blueant.com/archiva/internal"
                              ;; Using :env as a value here will cause an
                              ;; environment variable to be used based on
                              ;; the key; in this case LEIN_PASSWORD.
                              :username "milgrim" :password :env}]]

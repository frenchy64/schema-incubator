(def schema-version "1.4.0")
(defproject com.ambrosebs/schema-incubator "1.0.0-SNAPSHOT"
  :description "Extensions to Plumatic schema"
  :url "https://github.com/frenchy64/schema-incubator"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.8.0"]
                                  [org.clojure/clojurescript "1.10.520"]
                                  [prismatic/schema ~schema-version]
                                  [org.clojure/tools.nrepl "0.2.5"]
                                  [org.clojure/test.check "1.1.1"]
                                  [potemkin "0.4.1"]]
                   :eastwood {:exclude-namespaces []
                              :exclude-linters [:def-in-def :local-shadows-var :constant-test :suspicious-expression :deprecations
                                                :unused-meta-on-macro :wrong-tag :unused-ret-vals]}
                   :plugins [[lein-codox "0.10.8"]
                             [lein-cljsbuild "1.1.7"]
                             [lein-doo "0.1.10"]
                             [lein-pprint "1.3.2"]
                             [lein-shell "0.5.0"]
                             [jonase/eastwood "1.2.3"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"] [org.clojure/clojurescript "1.10.520"] [prismatic/schema ~schema-version]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.3"] [org.clojure/clojurescript "1.10.879"] [prismatic/schema ~schema-version]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.1"] [org.clojure/clojurescript "1.11.4"] [prismatic/schema ~schema-version]]}
             :1.12 {:dependencies [[org.clojure/clojure "1.12.0-master-SNAPSHOT"] [org.clojure/clojurescript "1.11.4"] [prismatic/schema ~schema-version]]
                    :repositories [["sonatype-oss-public" {:url "https://oss.sonatype.org/content/groups/public"}]]}}

  :aliases {"all" ["with-profile" "+dev:+1.9:+1.10:+1.11:+1.12"]
            "deploy" ["do" "clean," "deploy" "clojars"]
            "test" ["do" "clean," "test," "doo" "node" "test" "once"]
            "doc" ["codox"]}

  :jar-exclusions [#"\.swp|\.swo|\.DS_Store"]

  :source-paths ["src/clj" "src/cljc"]

  :test-paths ["test/clj" "test/cljc" "test/cljs"]

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src/clj" "src/cljc"]
                :compiler {:output-to "target/main.js"
                           :optimizations :whitespace
                           :pretty-print true}}
               {:id "test"
                :source-paths ["src/clj" "src/cljc"
                               "test/clj" "test/cljc" "test/cljs"]
                :compiler {:output-to "target/unit-test.js"
                           :main com.ambrosebs.schema-incubator.test-runner
                           :target :nodejs
                           :pretty-print true}}
               {:id "test-no-assert"
                :source-paths ["src/clj" "src/cljc"
                               "test/clj" "test/cljc" "test/cljs"]
                :assert false
                :compiler {:output-to "target/unit-test.js"
                           :main com.ambrosebs.schema-incubator.test-runner
                           :target :nodejs
                           :pretty-print true}}]}

  :codox {:source-uri "https://github.com/frenchy64/schema-incubator/blob/{git-commit}/{filepath}#L{line}"}

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag"]
                  ["deploy"]
                  ["shell" "./bin/push_docs_for_current_commit.sh"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]
                  ["shell" "git" "push" "origin" "main" "--tags"]])

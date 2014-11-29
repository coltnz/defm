(defproject defm "0.1.1"
            :url "http://example.com/FIXME"
            :license {:name "Eclipse Public License"
                      :url  "http://www.eclipse.org/legal/epl-v10.html"}
            :source-paths ["src/clj"]
            :java-source-paths ["src/java"]
            :javac-options ["-target" "1.7" "-source" "1.7"]
            :dependencies [[org.clojure/clojure "1.6.0"]
                           [org.clojure/core.match "0.2.1"]]
            :profiles {:dev {:source-paths ["dev"]
                             :dependencies [[org.clojure/core.match "0.2.1"]
                                            [defun "0.2.0-RC"]
                                            [prismatic/schema "0.3.2"]]}})

(defproject org.marianoguerra/clj-rhino "0.2.1"
  :description "library to ease the interaction between rhino and clojure"
  :url "http://github.com/marianoguerra/clj-rhino"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.mozilla/rhino "1.7R4"]]
  
  :javac-options      ["-target" "1.6" "-source" "1.6"]
  :source-paths       ["src/clj"]
  :java-source-paths  ["src/java"])

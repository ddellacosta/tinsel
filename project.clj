(defproject tinsel "0.5.0"
  :description "Selector-based templates with Hiccup."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2014"]
                 [hiccup "1.0.0"]
                 [hiccups "0.2.0"]
                 [hickory "0.5.2"]]

  :source-paths ["src"  "target/generated-src"]

  :plugins [[com.keminglabs/cljx "0.3.1"]]

  :hooks [cljx.hooks]

  :cljx {:builds [{:source-paths ["src"]
                   :output-path "target/generated-src"
                   :rules :clj}
                  {:source-paths ["src"]
                   :output-path "target/generated-src"
                   :rules :cljs}]})

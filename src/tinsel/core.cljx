(ns tinsel.core
  "Enlive-style templates with Hiccup."
  (:require [tinsel.utils :as utils]
            [tinsel.zip :as tzip]
            [hickory.core :as hickory]
            [clojure.zip :as zip]
            [clojure.walk :as walk]))

;;
;; Template Loading
;;

(defn html-document
  "Parse an HTML document out of the argument given, which can be either a
   string containing the HTML document, or a reader for that document."
  [string-or-reader]
  (let [html-string (if (string? string-or-reader)
                      string-or-reader
                      (slurp string-or-reader))]
    (hickory/parse html-string)))

(defn html-fragment
  "Parse an HTML fragment out of the argument given, which can be either a
   string containing the HTML fragment, or a reader for that fragment."
  [string-or-reader]
  (let [html-string #+clj (if (string? string-or-reader)
                            string-or-reader
                            (slurp string-or-reader))
                    #+cljs string-or-reader]
    (hickory/parse-fragment html-string)))

(defn hiccup-file
  "Parse hiccup forms out of the argument."
  [file-path]
  (vector (load file-path)))

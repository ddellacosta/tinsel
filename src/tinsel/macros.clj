(ns tinsel.macros
  (:require [hiccups.core :as hiccups]
            [hickory.select :as select]
            [tinsel.utils :as utils]
            [tinsel.zip :as tzip]
            [clojure.zip :as zip]
            [cljs.analyzer :as ana]))

;;
;; Compiler
;;
;; Note: Everything here runs at template compile time (that is, macro
;; expansion, and not during the template's render time).

(defn apply-transform
  "Given a transform (a selector function and a tranformation function), applies
   it where the selector dictates to the form given."
  [[select? transform] form]
  ;; Iterate through all the nodes in depth-first order, replacing any applicable.
  (loop [loc (tzip/postorder-first (tzip/hiccup-zip form))]
    ;; If this node is selected by the selector, transform it.
    (if (zip/end? loc)
      (zip/root loc)
      (recur (tzip/postorder-next
              (if (and (vector? (zip/node loc))
                       (select? loc))
                (zip/edit loc transform)
                loc))))))

(defn apply-transforms
  "transform-list is a list of pairs of functions. The first in each pair is a
   selector function; it returns true if the node is one of interest. The second
   is the transformer function; it is applied to nodes whose selector is true.
   The argument forms is a list of hiccup forms to apply all the transformations
   to in order."
  [transform-list forms]
  (if (empty? transform-list)
    forms
    (recur (rest transform-list)
           (doall (for [form forms]
                    (apply-transform (first transform-list) form))))))


;;
;; Transformers
;;
;; Note: Transformers are functions of a node that return the new version of
;; that node taking into account the user's desired transformation. They do
;; execute in the environment of the template, so template arguments are
;; available. However, keep in mind that the output is a hiccup form that
;; will be compiled and run later, at runtime. Code that should run at
;; template runtime should be expressed as a form in hiccup.

(defmacro set-content
  [new-content]
  (let [normalized-new-content (if (vector? new-content)
                                 (utils/normalize-form new-content)
                                 new-content)]
    `(fn [node#]
       (vector (utils/tag node#)
               (utils/attrs node#)
               (quote ~normalized-new-content)))))

(defmacro append-content
  [new-content]
  (let [normalized-new-content (if (vector? new-content)
                                 (utils/normalize-form new-content)
                                 new-content)]
    `(fn [node#]
       (conj node# (quote ~normalized-new-content)))))

(defmacro prepend-content
  [new-content]
  (let [normalized-new-content (if (vector? new-content)
                                 (utils/normalize-form new-content)
                                 new-content)]
    `(fn [node#]
       (apply vector
              (utils/tag node#)
              (utils/attrs node#)
              (quote ~normalized-new-content)
              (utils/contents node#)))))

(defmacro set-attrs
  [attr-map]
  `(fn [node#]
     (apply vector
            (utils/tag node#)
            `(merge (utils/attrs ~node#)
                    ~'~attr-map)
            (utils/contents node#))))

(defmacro deftemplate
  [tmpl-name source arg-list & transforms]
  (let [source (if (utils/code-form? source) ;; Need to force eval if source is
                 (eval source)               ;; not hiccup vectors.
                 source)
        source-forms (map utils/normalize-form ;; ["tag" {attrs} content...]
                          source)
        transforms (partition 2 (map eval transforms))
        transformed-forms (apply-transforms transforms source-forms)]
    `(defn ~tmpl-name
       ~arg-list
       (hiccups/html ~@transformed-forms))))

(ns ptonner.maniac
  (:require [clj-djl.ndarray :as nd]))

(def ndm (nd/base-manager))
(def x (nd/arange ndm 3))

(defn foo
  "I don't do a whole lot."
  [x]
  (prn x "Hello, World!"))

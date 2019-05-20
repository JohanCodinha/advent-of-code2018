(ns day13.core
  (:require [clojure.algo.generic.functor :refer [fmap]]))

(first
  (filter #(= "\\" %)
          (clojure.string/split 
            (first (clojure.string/split-lines (slurp "input.txt"))) #"")))


(def data
  (map
    (fn [l] (seq (char-array l)))
    (clojure.string/split-lines (slurp "input.txt"))))

(filter #(#{"<" ">" "v" "^"} %) (flatten data))

(def initial
  (reduce merge
  (flatten
    (map-indexed
      (fn [iy item]
        (map-indexed
          (fn [ix item]
            {[ix iy] item} 
            ) item)
        ) data))))



(ns day2.core)

(def input (clojure.string/split-lines (slurp "./input.txt")))

(defn string-has-x-of-any-letter [x str]
  ((complement empty?) (filter #(= x (second %)) (frequencies str))) 
  )

(apply *
(reduce
  (fn [[acc1 acc2] line]
    [(+ acc1 (if (string-has-x-of-any-letter 2 line) 1 0))
     (+ acc2 (if (string-has-x-of-any-letter 3 line) 1 0))] 
    )
  [0 0]
  input 
  ))


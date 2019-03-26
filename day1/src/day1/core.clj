(ns day1.core)

(def input (clojure.string/split-lines  (slurp "./input.txt")))


(reduce
  (fn [[hist acc] line]
    (let [num (Integer/parseInt line)
          freq (+ acc num)]
      (if (hist freq)
        (do (println (count hist)) (reduced freq))
        [(conj hist freq) freq]))
    )
  [(set nil) 0]
  (flatten (repeat input)))

(some #(if (> (second %) 1) (first %)) (frequencies [1 2 5 3 4 5]))
(pst)

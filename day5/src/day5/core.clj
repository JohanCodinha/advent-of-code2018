(ns day5.core
  (:require [clojure.string :as string]))

(def dummy "abBAcCcxyz")
(def input (butlast  (slurp "input.txt")))

(defn react? [a b]
  (when (and ((complement nil?) a) (( complement nil?) b))
    (= 32
       (Math/abs (- (int a) (int b))))))

(defn flow [s]
  (reduce
    (fn [acc item]
      (if (react? (peek acc) item)
        (pop acc)
        (conj acc item)))
    `()
    s))
(time
(def r
  (apply
    min
    (map
      (fn [char-code]
        (let [lower (char char-code)
              upper (char (- char-code 32 ))]
          (count
            (flow
              (remove #(or (= lower %) (= upper %)) input)))))
      (range (int \a) (int \z))))))



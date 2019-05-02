(ns day8.core)

(def dummy-inputs "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
(def inputs (slurp "input.txt")) 

(defn parse [inputs] (map #(Integer/parseInt %) (clojure.string/split inputs #" ")))

(pprint (second (parseNode (parse dummy-inputs))))

(defn parseNode
  [[quantity-child-nodes quantity-metadata & list-int]]
  (if (zero? quantity-child-nodes)
    [(drop quantity-metadata list-int) {:metadata (take quantity-metadata list-int)}]
    (let [childs (rest
                   (take
                     (+ 1 quantity-child-nodes)
                     (iterate (fn [[rest-int node]]
                                (parseNode rest-int))
                              [list-int {}])))
          [rest-int _] (last childs)]
      [(drop quantity-metadata rest-int)
      {:metadata (take quantity-metadata rest-int)
       :childs (map second childs)}]
      )) 
  )


(defn walk
  [node]
  [(:metadata node) (map walk (:childs node))])

(defn walk-2
  [node]
  (if (nil? (:childs node))
    (apply + (:metadata node))
    (map
      (fn [metadata]
        (walk-2 (nth (:childs node) (dec metadata))))
      (filter
        (fn [metadata]
          (or (= 0 metadata) (<= metadata (count (:childs node)))))
        (:metadata node)))))

(apply + (flatten (walk-2 (second (parseNode (parse (first (clojure.string/split-lines inputs))))))))

(apply + (flatten (walk (second (parseNode (parse (first (clojure.string/split-lines inputs))))))))

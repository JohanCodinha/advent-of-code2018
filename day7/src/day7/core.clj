(ns day7.core
  (:require
    [clojure.algo.generic.functor :as functor]))

(defn parse-input [text]
  (map
   (fn [l]
      (rest (re-find #"Step (\D) must be finished before step (\D)" l))) 
  (clojure.string/split-lines text)))

(def dummy (parse-input dummy-data))
(def inputs (slurp "input.txt"))


(defn roots [inputs]
  (remove
    (set (map second inputs))
    (set (map first inputs))))

(defn end-nodes [inputs]
  (first
    (remove
      (set (map first inputs) )
      (set (map second inputs)))))

(defn req [inputs]
  (functor/fmap
    #(map first %) 
    (group-by second inputs)))

(defn connected
  [inputs] (functor/fmap
             #(map second %) 
             (group-by first inputs)))

(defn hydrate [item stop dico]
  (if (= item stop)
    item
    [item (mapv (fn [leaf] (hydrate leaf stop dico)) (dico item))])  
  )

(defn part-1 [input-string]
  (let [inputs (parse-input input-string)
        required-by-job (req inputs)
        available-after-job (connected inputs)
        starters (roots inputs)
        end (end-nodes inputs)
        tree (map #(hydrate % end available-after-job) starters)
        path
        (loop [paths tree
               order []]
          (let [filtered-paths (remove #((set order) (first %)) (remove #(= end %) paths))
                possible (filter #(every? (set order) ((req puzzle) (first %))) filtered-paths)
                next (first (sort-by first possible))]
            (if (empty? filtered-paths)
              order
              (recur (into
                       (remove #(= (first next) (first %)) paths)
                       (last next))
                     (conj order (first next))))))]
    (apply str (conj path end))))

(part-1 inputs)

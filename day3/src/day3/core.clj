(ns day3.core
  (:use [clojure.algo.generic.functor :only (fmap)]))

(def parsed (clojure.string/split-lines (slurp "input.txt")))

(defn parseLine [str]
  (let [[_ id leftEdgeToFab topEdgeToFab width height]
        (re-matches #"\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" str)]
    (fmap
      #(Integer/parseInt %)
      {:id id
       :leftEdgeToFab leftEdgeToFab
       :topEdgeToFab topEdgeToFab
       :width width
       :height height})))

(defn area [{:keys [id width height topEdgeToFab leftEdgeToFab]}]
  {
   id
   (mapcat
     (fn [h]
       (map
         (fn [w]  [(+ topEdgeToFab h) (+ leftEdgeToFab w)])
         (range 1 (inc width))))
     (range 1 (inc height)))})

(pprint (area (parseLine (first parsed))))

(def expanded (mapcat area (map parseLine parsed)))

(defn intersect [dataset]
  (->> dataset
       frequencies 
       (filter (fn [[k v]] (> v 1)))
       (map first)))


(def areas (map area (map parseLine parsed)))

(def r
  (map
    (fn [[ id area]]
      (let [rest (dissoc (apply merge areas) id)
            rset (set (mapcat second rest))]
        (if (some rset area)
          nil
          id
          ))
      )
    (apply merge areas)))


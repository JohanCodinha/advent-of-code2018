(ns day6.core)

(defn input []
  (map
    (fn [l]
      (let [[x y] (map #(Integer/parseInt %) (rest (re-find #"(\d+), (\d+)" l)))]
        {:x x :y y}))
    (clojure.string/split-lines (slurp "input.txt"))))


(defn boundaries [coords]
  (let [min-x (:x (apply min-key :x coords))
        max-x (:x (apply max-key :x coords))
        max-y (:y (apply max-key :y coords))
        min-y (:y (apply min-key :y coords))]
    {:min-x min-x
     :min-y min-y
     :max-x max-x
     :max-y max-y}))

;{:min-x 54, :min-y 40, :max-x 357, :max-y 347}

(defn distance [{ax :x ay :y} {bx :x by :y}]
  (+
   (Math/abs (- ax bx))
   (Math/abs (- ay by))))

(defn grid [inputs]
  (let [coordinates inputs
        {:keys [min-x min-y max-x max-y]} (boundaries coordinates)]
    (for [y (range min-y max-y)]
      (for [x (range min-x max-x)]
        (let [closests (map (fn [coord] {:coord coord :dist (distance {:x x :y y} coord)}) coordinates)]
          {:pos {:x x :y y}
           :closests closests})))))

(def grid-p2
  (for [y (range (:min-y boundaries) (:max-y boundaries))]
    (for [x (range (:min-x boundaries) (:max-x boundaries))]
      (let [total-dist (reduce + (map #(distance {:x x :y y} %) input))]
        {:pos {:x x :y y} :total-dist total-dist}
        )
      )))



(def coordinates-at-border
  (distinct
    (map :closests
         (filter
           (fn [{{x :x y :y} :pos}]
             (or (= x (:min-x boundaries))
                 (= x (:max-x boundaries))
                 (= y (:min-y boundaries))
                 (= y (:max-y boundaries)))) 
           (remove #(> (count (apply min-key key (group-by :dist (:closests %)))) 1) (flatten (grid (input))))))))

(defn closests [{closests :closests}]
  (second
    (apply min-key key
           (group-by :dist closests))))

(defn boundaries->border [{:keys [min-x min-y max-x max-y]}]
   
  )
(closests (first (first (grid (input)))))

(def group (group-by :closest
                     (remove
                       (fn [location]
                             (or (> (count (closests location)) 1)
                                 ((set coordinates-at-border) location) 
                                 ))
                             (flatten grid)
                             )))

                             (count (val (apply max-key #(count (val %)) group)))

                             ;(map 
                             ;  (fn [x y] [x y])
                             ;  (flatten (repeat 3 (range 5)))
                             ;  (mapcat #(repeat 5 %) (range 3)))
                             ;
                             ;(mapcat identity
                             ;(for [y (range 3)]
                             ;  (for [x (range 5)]
                             ;  [x y])))

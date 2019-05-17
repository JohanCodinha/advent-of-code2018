(ns day12.core)

(defn parse
  [input]
  (let [lines (clojure.string/split-lines input)
        state (second (re-matches #"initial state: (.*)" (first lines)))
        notes (map #(rest (re-matches #"(.*) => (#|.)" %)) (drop 2 lines))]
    {:state state
     :notes notes}))

(def input (-> (slurp "input.txt")
               parse))

(defn generation
  [index state notes]
  (let [spread (clojure.string/join (map #(get state % ".") (range (- index 2) (+ index 3))))]
    (if (get notes spread)
      "#"
      ".")
    )
  )

(defn offset
  [state]
  (let [plants (filter #(= "#" (second %)) state)
        min-index (first (apply min-key first plants))
        max-index (first (apply max-key first plants))
        head-offset (into {} (map vector (range (- min-index 2) min-index) (repeat ".")))
        tail-offset (into {} (map vector (range (+ max-index 1) (+ max-index 2)) (repeat ".")))
        state-offset (merge state head-offset tail-offset)]
    state-offset 
    ))

(defn states
  [input]
  (let [notes (into #{} (keep #(and (= "#" (second %)) (first %)) (:notes input)))
        state (into {} (map-indexed vector (clojure.string/split (:state input) #"")))]
    (iterate
      (fn [s]
        (let [offset-state (offset s)]
          (into (sorted-map)
                (map (fn [[i _]]
                       [i (generation i offset-state notes)])
                     offset-state))
          ))
      state) 
    ))

;part2
(let [iter-while-not-equl
      (->> (states input)
           (map (fn [s]
                  (->> s
                       (filter #(= "#" (second %)))
                       (map first)
                       (reduce +))))
           (partition 3 1)
           (take-while
             (fn [[l c r]]
               (not
                 (and 
                   (=
                    (- c l)
                    (- r c))
                   (pos? (- c l))))))
           )
      [a b c] (last iter-while-not-equl)]
  (+ b
     (* (- c b)
        (- 50000000000
           (count iter-while-not-equl)))))

;Part1
(apply + (map first (filter #(= "#" (second %)) (last (take 21 (states input))))))

;Used for data analysis, looking for patern
(->>
  (map (fn [s]
         (->> s
              (filter #(= "#" (second %)))
              (map first)
              (reduce +)
              ))
       (states input))
  (partition 3 1)
  (filter (fn [[l c r]]
            (and (= 
                   (- c l)
                   (- r c))
                 (pos? (- c l)))))
  (map(fn [[l c r]]
        [l c r
         (- c l)
         (- r c)]))
  (take 10))


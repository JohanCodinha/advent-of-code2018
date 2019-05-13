(ns day10.core)

(def input
  (map 
    (fn [l]
      (let [[positionX positionY velocityX velocityY]
            (map #(Integer/parseInt %)
                 (rest
                   (re-matches
                     #"position=< ?(-?\d*),  ?(-?\d*)> velocity=< ?(-?\d*),  ?(-?\d*)>" l)))]
        {:posX positionX
         :posY positionY 
         :velX velocityX
         :velY velocityY}))
    (clojure.string/split-lines (slurp "input.txt"))))

(defn print-state
  [{:keys [minX minY maxX maxY]} state]
  (println minX maxX minY maxY)
  (doseq [y (range minY (inc maxY))]
    (print y)
    (doseq [x (range minX (inc maxX))]
      (if (some (fn [{:keys [posX posY]}]
                  (and (= posX x)
                       (= posY y))
                  ) state)
        (print "#")
        (print "."))) 
    (println "")
    )
  (println ""))

(defn add-velocity
  [{:keys [posX posY velX velY]}]
  {:posX (+ posX velX)
   :posY (+ posY velY)
   :velY velY
   :velX velX}
  )

(defn calc-area
  [state]
  (let [minX (:posX (apply min-key :posX state))
        minY (:posY (apply min-key :posY state))
        maxX (:posX (apply max-key :posX state))
        maxY (:posY (apply max-key :posY state))
        area (* (- maxX minX) (- maxY minY))]
    area))

(defn calc-box
  [l]
  (let [minX (:posX (apply min-key :posX l))
        minY (:posY (apply min-key :posY l))
        maxX (:posX (apply max-key :posX l))
        maxY (:posY (apply max-key :posY l))]
    {:minX minX
     :minY minY
     :maxY maxY
     :maxX maxX}))


(def data (loop [n 0
                 states [input]
                 ar (calc-area input)]
            (let [state (last states)
                  minX (:posX (apply min-key :posX state))
                  minY (:posY (apply min-key :posY state))
                  maxX (:posX (apply max-key :posX state))
                  maxY (:posY (apply max-key :posY state))
                  area (* (- maxX minX) (- maxY minY))
                  p-state (partial print-state minX minY maxX maxY)]
              (if (>= ar area)
                (recur (inc n)
                       (conj states (map add-velocity state))
                       area)
                states
                #_(dotimes [n 5] (p-state (map add-velocity state)))))
            ))
;Part 2
(- (count data) 2)
;Part 1
(map #(print-state (calc-box %) %) (take 5 (iterate #(map add-velocity %) (first (take-last 2 data)))))

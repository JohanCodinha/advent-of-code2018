(ns day11.core)

(let [grid-serial-number 7689]
  () 
  )

(defn grid
  [x y]
  (map vector
       (flatten (repeat (range 1 (inc x))))
       (mapcat #(repeat x %) (range 1 (inc y)))
       ))

(defn digit
  [n digit]
  (Math/abs (/ (- (mod n (* 10 digit)) (mod n digit)) digit)))

(defn power-level
  [grid-sn [x y]]
  (let [rack-id (+ x 10)]
    (-> rack-id 
        (* y)
        (+ grid-sn)
        (* rack-id)
        (digit 100)
        (- 5))))

(defn square
  [[x y] size]
  (map vector
       (mapcat #(repeat size %) (range x (+ x size)))
       (flatten (repeat (range y (+ y size))))))

(take 5 (into {} (map (fn [pos]
                   [pos 
                    (power-level 7689 pos)])
                 (grid 300 300))))
(defn flow
  [sq-size] 
  (let [g-sn 7689 
        square-size sq-size
        g (grid 300 300)
        gset (set g)
        cells (into {} (map (fn [pos]
                              [pos 
                               (power-level g-sn pos)])
                            g))]
    (apply max-key second
           (keep
             (fn [[pos _]]
               (let [square-pos (square pos square-size)
                     matching (every? gset square-pos)]
                 (when matching
                   [pos
                    #_(map (fn [p] [p (cells p)]) square-pos)
                    (apply + (map (fn [p] (cells p)) square-pos))]
                   ) 
                 ))
             cells))  
    ))
(pst)

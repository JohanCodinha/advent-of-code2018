(ns day9.core)

(def input "418 players; last marble is worth 70769 points")

(def input2 "10 players; last marble is worth 1618 points")
(def input3 "17 players; last marble is worth 1104 points")

(def toy-input "9 players; last marble is worth 25 points" )

(defn parse
  [string]
  (let [[players last-marbles] (map #(Integer/parseInt %) (rest (re-matches #"^(\d+).+ (\d+).+" string)))]
  {:players players
   :last-marbles last-marbles}))

(defn next-index
  [{:keys [marbles current]}]
  (let [index (+ 2 current)]
    (if (> index (count marbles))
      (- index (count marbles))
      index)))

(defn insert [v i e]  (vec (concat (subvec v 0 i) [e] (subvec v i))))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))


(defn flow
  [input]
  (let [inputs (parse input)
        players (:players inputs)
        last-marbles (:last-marbles inputs)
        rounds (flatten (repeat (range 1 (inc players))))
        initial {:marbles [0] 
                 :current 0
                 :marble 1}
        game (take-while #(not= (+ 2 last-marbles) (:marble %))
                     (iterate
                       (fn [circle]
                         (if (= 0 (mod (:marble circle) 23))
                           (let [removed-index (if (> 0 (- (:current circle) 7))
                                                 (- (count (:marbles circle)) 7)
                                                 (- (:current circle) 7))]
                             {:marbles (vec-remove (:marbles circle) removed-index)
                              :current removed-index
                              :marble (inc (:marble circle))
                              :score [(:marble circle) (get (:marbles circle) removed-index)]})
                           (let [next-index (next-index circle)]
                             {:marbles (insert (:marbles circle) next-index (:marble circle))
                              :current next-index
                              :marble (inc (:marble circle))})  
                           )) initial))]
    #_ (map (fn [[p s]]
           [p
           (apply +
           (map second s))])
    (group-by first
   (map (fn [[p c]] [p (apply +  (:score c))]) 
    (filter (fn [[ _ circle]] (:score circle))
            (map vector 
                 rounds
                 game
                 )
            ))))
    game
    ))


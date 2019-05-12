(ns day9.core
  ( :import (java.util ArrayDeque Deque)))

(def input "418 players; last marble is worth 70769 points")
(def input2 "10 players; last marble is worth 1618 points")
(def input3 "17 players; last marble is worth 1104 points")
(def toy-input "9 players; last marble is worth 25 points" )

(defn parse
  [string]
  (let [[players last-marbles] (map #(Integer/parseInt %) (rest (re-matches #"^(\d+).+ (\d+).+" string)))]
  {:players players
   :last-marbles last-marbles}))

(defn rotate!
  [dq n]
  (if (pos? n)
    (dotimes [_ n]
      (.addFirst dq (.removeLast dq)))
    (dotimes [_ (Math/abs n)]
      (.addLast dq (.removeFirst dq)))))

(defn marbles
  [players max-marble]
  (let [dq (doto (ArrayDeque.) (.addFirst 0))]
    (loop [n 1
           score {}]
      (if (<= n max-marble)
        (if (= 0 (mod n 23))
          (let [poped (do (rotate! dq 7) (.pollLast dq))
                player (mod n players)]
            (rotate! dq -1) 
            (recur (inc n)
                   (merge-with + score {player (+ poped n)})))
          (do
            (rotate! dq -1) 
            (.addLast dq n)
            (recur (inc n)
                   score)))
        score))))

;part-1
 (last (sort-by second (marbles 418 70769)))

;part-2
 (last (sort-by second (marbles 418 (* 100 70769))))

(ns day4.core
  (:use [java-time :only (local-time adjust units truncate-to fields time-between local-date local-date-time as)]
        [clojure.algo.generic.functor :only (fmap)]))

(def inputs (clojure.string/split-lines (slurp "input.txt")))

; "[1518-09-24 00:59] wakes up"
; "[1518-05-22 00:47] wakes up"
; "[1518-05-08 00:02] Guard #2719 begins shift"
; "[1518-11-12 00:00] Guard #3011 begins shift"
; "[1518-04-12 00:57] wakes up"

(defn parseTime [str]
  (local-date-time "yyyy-MM-dd HH:mm" str))

(parseTime "1518-04-12 00:57")

(defn parseMatches
  ([fm g1 g2] 
   (let [time (parseTime g1)] {:time time :activity g2}))
  ([fm g1 g2 g3 g4]
   (let [time (parseTime g1)] {:time time :guardId g3 :activity g4})))

(def parsed
  (map
    #(apply parseMatches
            (remove nil?
                    (re-matches #"\[(.+)\] (.+\#(\d+) (.+)|.+)" %)))
    inputs))

(def sorted (sort-by :time parsed))

(defn getPreviousKey [key index coll]
  (some key
        (reverse
          (take index coll))))

;(as (local-date 2015 9 20) :year)
(def guardHydrated
  (map-indexed
    (fn [index item]
      (let [guardId (or (:guardId item) (getPreviousKey :guardId index sorted))]
        (assoc item :guardId guardId)
        ))
    sorted))

(def sorted-by-guard
  (fmap
    #(reduce
       (fn [acc item]
         (if (= "begins shift" (:activity item))
           (conj acc [item])
           (conj (pop acc) (conj (peek acc) item))))
       []
       %)
    (group-by :guardId guardHydrated)))

(def res
  (map
    (fn [[guarId all-guard-shifts]]
      (let [minutes-sleeped (mapcat
                              (fn [item]
                                (let [fa (filter #(= (:activity %) "falls asleep") item)
                                      wu (filter #(= (:activity %) "wakes up") item)
                                      minutes (map #(range (as (:time %) :minute-of-hour) (as (:time %2) :minute-of-hour) ) fa wu)]
                                  minutes))
                              all-guard-shifts)
            most-common (first (sort-by val > (frequencies (flatten minutes-sleeped))))]
        {:guardId guarId
         :most-common (first most-common)
         :most-common-freq (last most-common)
         :total-sleep (count (flatten minutes-sleeped))
         }))
    sorted-by-guard))

(first (reverse (sort-by :most-common-freq res)))
{:guardId "2719", :most-common 36, :most-common-freq 18, :total-sleep 378}



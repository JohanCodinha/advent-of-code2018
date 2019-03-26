(ns day4.core
  (:require
    [java-time :only (local-time adjust units truncate-to fields time-between local-date local-date-time as)]
    [clojure.algo.generic.functor :as functor]))

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
  ([fm timestamp activity] 
   (let [parsedTime (parseTime timestamp)] {:time parsedTime :activity activity}))
  ([fm timestamp g2 guardId activity]
   (let [parsedTime (parseTime timestamp)] {:time parsedTime :guardId guardId :activity activity})))

(def parsed
  (map
    #(->> (re-matches #"\[(.+)\] (.+\#(\d+) (.+)|.+)" %)
          (remove nil?)
          (apply parseMatches))
    (sort inputs)))


(defn getPreviousKey [key index coll]
  (some key
        (reverse
          (take index coll))))


(def guardHydrated
  (map-indexed
    (fn [index item]
      (let [guardId (or (:guardId item) (getPreviousKey :guardId index parsed))]
        (assoc item :guardId guardId)
        ))
    parsed))


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
      (let [minutes-sleeped
            (mapcat
              (fn [item]
                (let [fa (filter #(= (:activity %) "falls asleep") item)
                      wu (filter #(= (:activity %) "wakes up") item)
                      minutes (map #(range (as (:time %) :minute-of-hour) (as (:time %2) :minute-of-hour) ) fa wu)]
                  minutes))
              all-guard-shifts)
            most-common
            (first (sort-by val > (frequencies (flatten minutes-sleeped))))]
        {:guardId guarId
         :most-common (first most-common)
         :most-common-freq (last most-common)
         :total-sleep (count (flatten minutes-sleeped))
         }))
    sorted-by-guard))

(last (sort-by :most-common-freq res))
{:guardId "2719", :most-common 36, :most-common-freq 18, :total-sleep 378}



(ns clj.day-4.core
  (:require
   [clojure.edn :as edn]
   [clojure.string :as string]))

(def my-input (slurp "resources/day_4/input"))
(def input-lines (string/split-lines my-input))

(defn encoded-interval->numerical-interval [s]
  (let [string-pair (string/split s #"-")]
    (mapv edn/read-string string-pair)))

(defn line->intervals [line]
  (let [encoded-ranges (string/split line #",")]
    (map encoded-interval->numerical-interval encoded-ranges)))

(defn interval-covered [[s1 e1] [s2 e2]]
  (and
   (<= s1 s2 e1)
   (<= s1 e2 e1)))

(defn interval-covered-sym [[i1 i2]]
  (or
   (interval-covered i1 i2)
   (interval-covered i2 i1)))

(def redundant-assignments
   (->> input-lines
        (map line->intervals)
        (filter interval-covered-sym)))

(comment
  input-lines

  (encoded-interval->numerical-interval "3-5")
  ;; => [3 5]

  (line->intervals "1-3,4-5")
  ;; => ([1 3] [4 5])

  (interval-covered [1 5] [2 4])
  ;; => true

  (interval-covered [1 5] [2 6])
  ;; => false

  (interval-covered [1 5] [1 5])
  ;; => true

  (interval-covered-sym [[2 3] [1 5]])
  ;; => true

  redundant-assignments

  (count redundant-assignments)
  ;; => 477

  )

(defn interval-overlap [[s1 e1] [s2 e2]]
  (or
   (<= s1 s2 e1)
   (<= s1 e2 e1)))

(defn interval-overlap-sym [[i1 i2]]
  (or
   (interval-overlap i1 i2)
   (interval-overlap i2 i1)))

(def redundant-assignments-2
  (->> input-lines
       (map line->intervals)
       (filter interval-overlap-sym)))

(comment
  
  redundant-assignments-2
  
  (count redundant-assignments-2)
  ;; => 830

  )
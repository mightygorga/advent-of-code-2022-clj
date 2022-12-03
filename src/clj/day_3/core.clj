(ns clj.day-3.core
  (:require 
   [clojure.set]
   [clojure.string :as string]))


(def my-input (slurp "resources/day_3/input"))
(def input-lines (string/split-lines my-input))

(defn split-compartments [s]
  (let [half-size (/ (count s) 2)]
    [(take half-size s) (take-last half-size s)]))

(defn find-common [[c1 c2]]
  (clojure.set/intersection (set c1) (set c2)))

(def lowercase-offset (- (int \a) 1))
(def uppercase-offset (- (int \A) 27))

(defn char->priority [c]
  (let [char-as-int (int c)
        is-uppercase (> (int \a) char-as-int)
        offset (if is-uppercase
                 uppercase-offset
                 lowercase-offset)]
    (- char-as-int offset)))

(def total-error-priorities
  (->> input-lines
       (map (comp char->priority first find-common split-compartments))
       (reduce +)))

(comment
  input-lines
  (def test-string "fijUoFiH")

  (take 4 test-string)
  (take-last 4 test-string)

  (split-compartments test-string)
  
  (-> test-string
      split-compartments
      find-common)
  ;; => #{\i}
  
  (map (comp char->priority first find-common split-compartments) input-lines)
  
  (int \a)
  ;; => 97
  
  (int \A)
  ;; => 65
  
  (char->priority \a)
  ;; => 1

  (char->priority \z)
  ;; => 26

  (char->priority \A)
  ;; => 27 
  
  (char->priority \Z)
  ;; => 52
  
  total-error-priorities
  ;; => 7763

  )

(defn find-common-group-batch [[r1 r2 r3]]
  (clojure.set/intersection
   (into #{} r1)
   (into #{} r2)
   (into #{} r3)))

(def total-batch-value
  (->> input-lines
       (partition 3)
       (map (comp char->priority first find-common-group-batch))
       (reduce +)))

(comment
  
  (partition 3 input-lines)
  
  (find-common-group-batch ["asv" "bsi" "ics"])
  ;; => #{\s}

  
  total-batch-value
  ;; => 2569 
  
  )
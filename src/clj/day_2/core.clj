(ns clj.day-2.core
  (:require
   [clojure.string :as string]))

(def my-input (slurp "resources/day_2/input"))
(def input-lines (string/split-lines my-input))

(defn code->shape [code]
  (case code
    ("A" "X") :rock
    ("B" "Y") :paper
    ("C" "Z") :scissors))

(def code->expected-result
  {"X" :loss
   "Y" :draw
   "Z" :win})

(def result->points
  {:loss 0
   :draw 3
   :win 6})

(defn string-instructions->rounds [s]
  (let [string-choices (string/split s #" ")]
    (map code->shape string-choices)))

(defn string-instructions->fixture [s]
  (let [string-choices (string/split s #" ")
        [their-code result-code] string-choices]
    (list (code->shape their-code) (code->expected-result result-code))))

(def shape->points
  {:rock 1
   :paper 2
   :scissors 3})

(defn round->shape-points [[_ mine]]
  (shape->points mine))

(def defeats
  #{'(:rock :scissors)
    '(:paper :rock)
    '(:scissors :paper)})

(defn round->result [[yours mine :as round]]
  (if (= yours mine)
    :draw
    (if (defeats round)
      :loss
      :win)))

(defn fix-result-1 [[theirs expected-result]]
  (let [my-options [:rock :paper :scissors]
        potential-plays (map (partial list theirs) my-options)]
    (some #(when (= expected-result (round->result %)) %) potential-plays)))

(def fix-result (memoize fix-result-1))

(def round->result-points (comp result->points round->result))

(defn round->points [round]
  (let [shape-points (round->shape-points round)
        result-points (round->result-points round)]
    (+ shape-points result-points)))

(def total-score-strat-1
  (->> input-lines
       (map string-instructions->rounds)
       (map round->points)
       (reduce +)))

(def total-score-strat-2
  (->> input-lines
       (map string-instructions->fixture)
       (map fix-result)
       (map round->points)
       (reduce +)))

(comment
  my-input
  input-lines
  (string-instructions->rounds "A X")
  (map string-instructions->rounds input-lines)

  (round->shape-points [:paper :paper])
  ;; => 2
  
  (round->points '(:scissors :scissors))
  ;; => 6

  (round->result '(:rock :rock))
  ;; => :draw

  (round->result '(:rock :paper))
  ;; => :win

  total-score-strat-1
  ;; => 9177

  (fix-result '(:rock :draw))
  ;; => (:rock :rock)

  (fix-result '(:rock :win))
  ;; => (:rock :paper)

  total-score-strat-2
  ;; => 12111

  )
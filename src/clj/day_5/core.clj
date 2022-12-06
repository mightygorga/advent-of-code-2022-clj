(ns clj.day-5.core
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]))


(def my-input (slurp "resources/day_5/input"))

(def partitioned-input
  (-> my-input
      (string/split #"\n\n"))
  )

(def value-positions (range 1 34 4))

(defn line-vec->row [v]
  (mapv (partial get v) value-positions))

(defn read-vertically [seq-of-vecs]
  (mapv
   (fn [i]
     (mapv #(get % i) seq-of-vecs))
   (range 0 9)))

(def initial-containers
  (->> partitioned-input
       first
       string/split-lines
       butlast
       (map (partial into [] ))
       reverse
       (map line-vec->row)
       (read-vertically)
       (mapv (partial filterv #(not= \space %)))))

(def instruction-list 
  (->> partitioned-input
       second
       string/split-lines))

(def instruction-regex
  #"move (\d+) from (\d+) to (\d+)")

(defn parse-string-instructions [s]
  (let [[_ string-amount from to] (re-find instruction-regex s)
        amount (edn/read-string string-amount)]
    (repeat amount [(edn/read-string from) (edn/read-string to)])))

(def simple-instruction-list
  (->> instruction-list
       (map parse-string-instructions)
       (mapcat identity)))

(defn apply-move-instruction [stacks instruction]
  (let [[from to] instruction
        
        from-index (dec from)
        to-index (dec to)
        moving-item (peek (get stacks from-index))
        stacks-with-removed-item (update stacks from-index pop)]
    (if moving-item
      (update stacks-with-removed-item to-index conj moving-item)
      stacks)))

(def reshuffled-stacks
  (reduce apply-move-instruction initial-containers simple-instruction-list))

(def answer
  (->> reshuffled-stacks
       (map peek)
       (string/join)))

(comment
  
  (-> my-input
      (string/split  #"\n\n")
      first
      string/split-lines)
  (range 1 34 4)
  
  (peek [1 2 3])
  
  (conj [1 2 3] 4)
  
  (first (string/split my-input #"\n\n"))
  initial-containers
  
  (re-find instruction-regex "move 5 from 8 to 1")
  instruction-list
  
  simple-instruction-list
  
  (apply-move-instruction initial-containers [2 3])
  
  reshuffled-stacks
  
  answer
  ;; => "TLFGBZHCN"

  )

(defn parse-string-instructions-9001 [s]
  (let [[_ amount from to] (re-find instruction-regex s)]
    [(edn/read-string amount) (edn/read-string from) (edn/read-string to)]))

(def instruction-list-9001
  (->> instruction-list
       (map parse-string-instructions-9001)))

(defn remove-n-from-end [v n]
  (let [end (- (count v) n)
        end-in-bounds (max 0 end)]
    (subvec v 0 end-in-bounds)))

(defn apply-move-instruction-9001[stacks instruction]
  (let [[amount from to] instruction
        from-index (dec from)
        to-index (dec to)
        moving-items (take-last amount (get stacks from-index))
        moving-item-count (count moving-items)
        stacks-with-removed-items (update stacks from-index remove-n-from-end moving-item-count)]
    (if (some? (seq moving-items))
      (update stacks-with-removed-items to-index into moving-items)
      stacks)))

(def reshuffled-stacks-9001
  (reduce apply-move-instruction-9001 initial-containers instruction-list-9001))

(def answer-9001
  (->> reshuffled-stacks-9001
       (map peek)
       (string/join)))

(comment
  
  instruction-list-9001
  
  (take-last 2 [0 1 2 3])
  ;; => (2 3)
  
  (into [] '(2 3))
 
  initial-containers
  (apply-move-instruction-9001 initial-containers [7 2 3])
  
  answer-9001
  ;; => "QRQFHFWCL"
  
  )
(ns day-1.core
  (:require
   [clojure.edn :as edn]
   [clojure.string :as string]))

(def my-input (slurp "resources/day_1/input"))

(defn sum-strings [string-list]
  (->> string-list
       (map edn/read-string)
       (reduce +)))

(def calories-per-elf
  (->> my-input
       string/split-lines
       (partition-by string/blank?)
       (filter #(not= '("") %))
       (map sum-strings))
  )

(def total_calories
  (reduce max calories-per-elf))

(def top-3-calories-sum
  (->> calories-per-elf
       (sort #(compare %2 %1))
       (take 3)
       (reduce +)))

(comment
  
  (sort #(compare %2 %1) calories-per-elf)
  
  total_calories
  ;; => 70509

  top-3-calories-sum
  ;; => 208567

  )
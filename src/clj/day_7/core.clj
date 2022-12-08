(ns clj.day-7.core
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]))


(def my-input (slurp "resources/day_7/input"))
(def input-lines (string/split-lines my-input))

(def cd-regex #"^\$ cd (.+)$")
(def ls-command-regex #"^\$ ls$")
(def ls-output-dir-regex #"^dir (.+)$")
(def ls-output-file-regex #"^(\d+) (.+)$")

(def initial-dir-state {:files {} :dirs #{}})
(def initial-shell-state {:wd [] :tree {}})

(defn apply-cd-command [arg state]
  (let [op (case arg
             "/" (constantly [])
             ".." pop
             #(conj % arg))] 
    (update state :wd op)))

(defn apply-ls-command [{:keys [wd] :as state}]
  (assoc-in state [:tree wd] initial-dir-state))

(defn add-dir-to-wd [dir-name state]
  (let [{:keys [wd]} state]
    (update-in state [:tree wd :dirs] conj dir-name)))

(defn add-file-to-wd [file-name file-size state]
  (let [{:keys [wd]} state]
    (update-in state [:tree wd :files] assoc file-name file-size)))

(defn apply-shell-line [state line]
  (condp re-matches line
    
    cd-regex
    :>>
    (fn [[_ arg]] (apply-cd-command arg state))
    
    ls-command-regex
    (apply-ls-command state)
    
    ls-output-dir-regex
    :>>
    (fn [[_ dir-name]] (add-dir-to-wd dir-name state))
    
    ls-output-file-regex
    :>>
    (fn [[_ file-size name]]
      (add-file-to-wd name (edn/read-string file-size) state))))

(def shell-state
  (reduce apply-shell-line initial-shell-state input-lines))

(defn files->dir-size [files]
  (->> files
       vals
       (reduce +)))

(defn all-prefixes [v]
  (take (inc (count v)) (iterate pop v)))

(defn add [l r]
  (+ (or l 0) (or r 0)))

(defn accumulate [tree [path size]]
  (let [path-prefixes (all-prefixes path)]
    (reduce
     (fn [acc pp] 
       (update acc pp add size))
     tree
     path-prefixes)))

(def dir-file-sizes
  (->> shell-state
       :tree
       (map (fn [[k v]] [k (-> v :files files->dir-size)]))))

(def inclusive-dir-sizes
  (reduce accumulate {} dir-file-sizes))

(comment
  input-lines

  (re-matches cd-regex "$ cd /")
  (re-matches ls-command-regex "$ ls")
  (= (first input-lines) "$ cd /")
  (re-matches cd-regex (first input-lines))

  (apply-cd-command "ed" {:wd ["a" "b"]})
  ;; => {:wd ["a" "b" "ed"]}
  (apply-cd-command "/" {:wd ["a" "b"]})
  ;; => {:wd []}
  (apply-cd-command ".." {:wd ["a" "b"]})
  ;; => {:wd ["a"]}

  (apply-ls-command {:wd ["a" "b"]})
  ;; => {:wd ["a" "b"], :tree {["a" "b"] {:files {}, :dirs #{}}}}

  (add-dir-to-wd "foo" {:wd ["a"] :tree {["a"] {:dirs #{}}}})
  ;; => {:wd ["a"], :tree {["a"] {:dirs #{"foo"}}}}

  (add-file-to-wd "foo.txt" 6534 {:wd ["a"]})
  ;; => {:wd ["a"], :tree {["a"] {:files {"foo.txt" 6534}}}}

  (first input-lines)
  ;; => "$ cd /"

  (apply-shell-line initial-shell-state "$ cd /")

  (-> shell-state
      :tree
      (get []))
  ;; => {:files {"fddw.bgw" 169838, "jtwpn.lnr" 248637, "lnmrrht.zbn" 319470, "pqpslbtn" 99548, "rqpt" 102720},
  ;;     :dirs #{"rbztmqjn" "fvhmzqc" "hqmlnpn" "bgmjrlz" "cbcwz" "bhp" "vfrtt"}}


  (->> shell-state
       :tree
       (map (fn [[k v]] [k (-> v :files files->dir-size)])))

  (all-prefixes [1 2 3])

  (accumulate {} [["foo" "bar"] 42])
  
  (->> inclusive-dir-sizes
       vals
       (filter #(>= 100000 %))
       (reduce +))
  ;; => 1555642

  )

(def total-space-used
  (get inclusive-dir-sizes []))

(def free-space
  (- 70000000 total-space-used))

(def must-free
  (- 30000000 free-space))

(comment
  total-space-used
  ;; => 45349983
  free-space
  ;; => 24650017
  must-free
  ;; => 5349983
  (->> inclusive-dir-sizes
       vals
       (filter #(<= must-free %))
       (reduce min))
  ;; => 5974547
  )
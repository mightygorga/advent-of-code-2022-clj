(ns clj.day-6.core)

(def my-input (slurp "resources/day_6/input"))

(def char-vec (vec my-input))

(def buffer-length 4)

(defn buffer-pos-at [v idx]
  (let [buffer-candidate (subvec v idx (+ idx buffer-length))
        distinct-in-buffer (count (distinct buffer-candidate))]
    (when (= buffer-length distinct-in-buffer)
      idx)))

(defn find-buffer-pos [v]
  (let [last-possible-index (- (count v) buffer-length)
        is-buffer-pos (partial buffer-pos-at v)
        indexes (range 0 last-possible-index)
        programmer-idx (some is-buffer-pos indexes)]
    (when programmer-idx
      (+ programmer-idx buffer-length))))

(comment
  (distinct (vec "1231"))
  
  (subvec [1 2 3 4 5 6] 2 6)
  
  my-input
  char-vec
  
  (find-buffer-pos char-vec)
  ;; => 1134

  

  )
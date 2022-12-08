(ns clj.day-6.core)

(def my-input (slurp "resources/day_6/input"))

(def char-vec (vec my-input))


(defn buffer-pos-at [buffer-length v idx]
  (let [buffer-candidate (subvec v idx (+ idx buffer-length))
        distinct-in-buffer (count (distinct buffer-candidate))]
    (when (= buffer-length distinct-in-buffer)
      idx)))

(defn find-buffer-pos [buffer-length v]
  (let [last-possible-index (- (count v) buffer-length)
        is-buffer-pos (partial buffer-pos-at buffer-length v)
        indexes (range 0 last-possible-index)
        programmer-idx (some is-buffer-pos indexes)]
    (when programmer-idx
      (+ programmer-idx buffer-length))))

(comment
  (distinct (vec "1231"))
  
  (subvec [1 2 3 4 5 6] 2 6)
  
  my-input
  char-vec
  
  (find-buffer-pos 4 char-vec)
  ;; => 1134

  (find-buffer-pos 14 char-vec)
  ;; => 2263


  )
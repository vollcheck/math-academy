(ns numeric-tower)

(set! *unchecked-math* :warn-on-boxed)

(defn factorial ^BigInteger [^long n]
  (loop [result 1N
         n (bigint n)]
    (if (zero? n) ;; what do I need to compare?
      result
      (recur (*' result n)
             (unchecked-dec-int n)))))

(defn permutations [n r]
  (/ (factorial n)
     (factorial (- n r))))

(def p permutations)
(def P permutations)

(defn combinations [n r]
  (/ (factorial n)
     (* (factorial r)
        (factorial (- n r)))))

(def c combinations)
(def C combinations)

(comment
  (p 30 3)

  (defn range-in
    "inclusive-end range"
    [start end]
    (range start (inc end)))

  (defn product [coll]
    (reduce * 1 coll))

  (/ (product (range 10 13))
     (product (range 28 31)))

  (reduce * 1 (range 10 13))

  (combinations 30 3)
  ;; => 4060N

  (combinations 12 3)
  ;; => 220N


  ;; Exercise description:
  ;; Jim randomly draws 3 green marbles from a bag containing 30 marbles.
  ;; What is the probability of this event, given that there were 12 identical
  ;; green marbles in the bag?
  (/ (c 12 3)
     (c 30 3))
  ;; => 11/203

  ;; also, this can be shortened as follows:
  (/ (product (range-in 10 12))
     (product (range-in 28 30)))

  ;; so, clearly, there's room for optimizations!

  )

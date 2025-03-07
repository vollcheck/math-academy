(ns core)

(defn product
  ([coll]
   (reduce * 1 coll))
  ([start end]
   (product (range start end))))

;; NOTE: food for thought: maybe it is better to handle factorial function
;; as a reducing function rather than dynamic programming/recursive one?
;; to be optimized

(defn fact
  ;; "NOTE: that should be used only in "full collection" cases e.g. from number
  ;; n to 1 if you have a subtracted range, please use `product` function"
  [n]
  (if (= 0 n)
    1
    (* n (fact (dec n)))))

(defn p
  "number of permutations of 'n' objects taken 'r' at a time"
  [n r]
  ;; TODO: that could be optimized by subtracting subfactorials
  #_(/ (fact n) (product (inc r) (inc n)))
  (/ (fact n) (fact (- n r))))

(defn combi [n r]
  (let [diff (- n r)
        fraction (product (inc r) (inc n))
        denominator (fact diff)]
    (/ fraction denominator)))


(comment
  (p 20 4)
  ;; => 116280
  (p 11 3)
  ;; => 990

  ;; 05.03.2025
  (-> (range 16 21)
      product
      (/ 120))
  ;; => 15504

  (fact 3)
  ;; => 6

  (combi 6 3)

  (combi 36 33)
  (combi 49 47)
  (combi 9 2)
  (combi 8 2)
  (combi 10 8)
  (combi 26 20)
  ;; => 230230


  (/ (product 34 37)
     (fact 3))


  (product 5 (inc 8))

  (p 8 4)
  ;; => 1680



  )

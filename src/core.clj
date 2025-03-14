(ns core)

(defn f
  ([floating-number]
   (format "%.2f" floating-number))
  ([floating-number scale]
   (format (str "%." scale "f") floating-number)))

(comment
  ;; doesnt work for now :///
  ;;(println #f 1321321.1321)
  )

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
  (if (< n 0)
    (throw (IllegalArgumentException. "fact not defined for negative numbers"))
    (loop [i 1N
           acc 1N]
      (if (> i n)
        acc
        (recur (inc i) (* acc i))))))

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

(def c combi)


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

  (/ (product 7 11)
     (Math/pow 10 4))
  ;; => 0.504

  (def x1 (fact 7))
  ;; => 5040

  (def x2 (* (fact 2) (fact 5)))
  ;; => 240

  (/ x1 x2)
  ;; => 21


  (/ (fact 15)
     (* (fact 2) (fact 13)))
  ;; => 105

  (defn c [n r]
    (let [diff (- n r)]
      (/ (product diff (inc n))
         (fact r))))

  ;; drawing 2 balls from the bag of 15 balls.
  (def c1 (c 15 2))
  ;; => 105

  (def c2 (c 8 2))
  ;; => 28

  (defn full-c [all relevant r]
    (/ (c relevant r)
       (c all r)))

  (full-c 15 8 2)
  ;; => 4/15

  (/ 28 105)

  ;; QUESTION 5

  (c 27 5)
  ;; N => 1776060

  ;; n1 (3 boys out of 15 )
  (c 15 3)
  ;; => 5460

  ;; n2 (2 boys out of 12)
  (c 12 2)
  ;; => 660

  (* 660 5460)
  ;; => 3603600

  (/ 3603600
     1776060)

  (defn c2 [n r]
    ;; we might use the heuristics that the 'r' should be lower than 'n' so we can subtract
    ;; and then create a sub-factorial
    (/ (fact n)
       (* (fact r) (fact (- n r)))))

  (def n1 (c2 15 3))
  ;; => 455

  (def n2 (c2 12 2))
  ;; => 66

  (def N (c2 27 5))

  (float (/ (* n1 n2)
            N))
  ;; => 0.37198067


  (def N (c2 18 3))
  (def n1 (c2 10 2))
  (def n2 (c2 8 1))
  (-> (* n1 n2)
      (/ N)
      float)


  ;; QUESTION 7
  (def N (Math/pow 12 4))
  (def p1 (p 12 4))

  (def A' (/ p1 N))
  (- 1 A')

  ;; QUESTION 8
  (def N (Math/pow 6 4))
  (def p1 (p 6 4))


  )

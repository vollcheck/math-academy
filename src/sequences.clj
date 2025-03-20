(ns seqeuences
  (:require [clojure.math :as m]))

(defn range-in [start end]
  (range start (inc end)))

(defn sigma
  [start end f]
  (transduce
   (map f)
   + 0 (range-in start end)))

(comment
  (def f #(- (* 3 % %)
             (* 5 %)))
  (map f (range 10 (inc 12)))
  ;; => (250 308 372)

  (def f #(* % % (+ % 1)))
  (map f (range-in 3 6))
  ;; => (36 80 150 252)

  (def f #(* 4 (m/pow 4 (dec %))))
  (sigma 3 5 f)

  (def f #(/ (/ 1 (m/pow 4 (dec %)))
             2))
  (rationalize (sigma 2 4 f)) ;; NOTE: maaaan, what a great function!
  ;; => 21/128



  )

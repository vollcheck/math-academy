(ns statistics
  (:require [clojure.math :as m]))

(defn mean
  [coll]
   (/ (reduce + 0 coll)
      (count coll)))

(def avg mean)

(defn variance
  ([coll] (variance coll {:debug false :r false}))
  ([coll opts]
   (let [mean (mean coll)
         xform (map #(m/pow (- % mean) 2))
         std-diff (transduce xform + 0 coll)
         cnt (count coll)]
     (cond
       (:debug opts) [std-diff cnt]
       (:r opts) (rationalize (/ std-diff cnt))
       :else (/ std-diff cnt)))))

(defn variance2 [coll]
  (let [coll-pow (map #(m/pow % 2) coll)
        mean-pow (mean coll-pow)
        pow-mean (m/pow (mean coll) 2)]
    (- mean-pow pow-mean)))

(defn z-index [x mean variance]
  (/ (- x mean) (m/sqrt variance)))


(comment
  (z-index 21.5 25 4.18)
  (z-index 14 28 3.60)
  (/ 5.5 2.2)
  (- 12.04 13.3)
  ;; => -1.2600000000000016
  (/ -1.26 -0.9)
  (/ 16 2.5)

  (variance [1 7 3 9])
  ;; => 10.0
  (variance2 [1 7 3 9])
  ;; => 10.0
  (variance [3 -8 -2 5 -4 -6])
  (variance [-5 4 -1 -9 1] {:r true})

  (let [mean (/ -15 5)
        pow-mean (/ 295 5)]
    (- pow-mean (m/pow mean 2)))
  ;; => 50.0

  (let [mean (/ 35 7)
        pow-mean (/ 201 7)]
    [(m/pow mean 2) pow-mean])
  ;; => [25.0 201/7]

  (variance [-2 1 -5 3 -7] {:debug true})
  (* 88 9)
  ;; => 792

  (* 5 25 )
  ;; => 125

  (- 792 125)
  ;; => 667
  (rationalize (/ 667 45))

  (def c [-9 1 2 10 -1 3])
  (mean c)
  ;; => 1

  (apply + (map #(m/pow % 2) c))
  ;; => 196.0/6
  190/6
  95/3


  )

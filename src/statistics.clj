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

(defn sum-of-squares [coll1 coll2]
  (assert (count coll1) (count coll2))
  (let [n (count coll1)
        mean1 (mean coll1)
        mean2 (mean coll2)
        diffs1 (mapv #(- % mean1) coll1)
        diffs2 (mapv #(- % mean2) coll2)]
    (->> (map * diffs1 diffs2)
         (reduce + 0))))

(defn covariance [coll1 coll2]
  (/ (sum-of-squares coll1 coll2)
     (count coll1)))

(defn sum-by [xf coll]
  (transduce xf + 0 coll))

(defn mean-estimate
  "estimates a mean based on vec of vecs [midpoint frequency]"
  [vecs]
  (let [n (sum-by (map second) vecs)
        xf (map (fn [[midpoint frequency]] (* midpoint frequency)))]
    (-> (sum-by xf vecs)
        (/ n)
        float)))

(defn variance-estimate
  "again, vec of vecs [midpoint frequency]"
  [vecs]
  (let [n (sum-by (map second) vecs)
        xf (map (fn [[midpoint frequency]] (* midpoint frequency)))
        mean (-> (sum-by xf vecs) (/ n) float)]
    (-> (transduce
         (map (fn [[midpoint frequency]]
                (-> (- midpoint mean)
                    (m/pow 2)
                    (* frequency))))
         + 0
         vecs)
        (/ n))))

(defn std-dev-estimation [vecs]
  (m/sqrt (variance-estimate vecs)))

(comment
  "17.04.2025"
  (sum-of-squares [123 122 121 120 125 127]
                  [102 111 110 105 106 108])
  (float (/ 69 15))
  (float (/ 56 16))

  (sum-of-squares [])
  )


(comment
  "16.04.2025"
  (mean-estimate [[415 1]
                  [445 3]
                  [475 2]
                  [505 2]])
  ;; => 463.75

  (def data1 [[15 30]
              [25 20]
              [35 50]])
  (variance-estimate data1)
  ;; => 76.0
  (std-dev-estimation data1)
  ;; => 8.717797887081348

  (def data2 [[7.5 6]
              [17.5 4]
              [22 2]
              [27 3]])
  (variance-estimate data2)
  ;; => 58.5

  (def data3 [[5 2]
              [15 4]
              [25 4]
              [35 5]])
  (variance-estimate data3)
  ;; => 109.33333333333333
  (std-dev-estimation data3)
  ;; => 10.456258094238748
  )

(comment
  "01.04.2025"
  (covariance [6 -2 7 1] [5 -1 4 0])
  (covariance [4 4 -4 4] [-1 -2 7 4])
  (covariance [8 5 9 10] [71 55 81 97])
  )

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

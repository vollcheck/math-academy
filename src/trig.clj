(ns trig
  (:require [clojure.math :as m]
            [core :refer [f]]))



(comment
  (-> 25
      m/to-radians
      m/cos
      (* 14)
      f)
   ;; => "12.69"


  (-> 47
      m/to-radians
      m/cos
      (* 19)
      (f 0))
  ;; => "13"


  ;; TODO: it would be nice to be able to write
  ;; my formulas like this: 19 cos(47) * 14
  ;; that would require dumb splitting by space or prepare a parser
  ;; the latter option is more interesting to implement

  (-> 56
      m/to-radians
      m/tan
      (* 9)
      (f 0))
   ;; => "13"

  (-> (/ 2.91
         (-> 20
             m/to-radians
             m/tan))
      f)
   ;; => "8.00"


  )

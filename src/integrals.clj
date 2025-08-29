(ns integrals)

(defn left-riemann-sum [f [start stop] step]
  (let [xes (range start stop step)]
    (reduce
     (fn [accumulator x]
       (let [height (f x)
             ;; step is a rectangle base here
             area (* height step)]
         (+ accumulator area)))
     0
     xes)))

(def lrs left-riemann-sum)

(comment
  ;; would be nice to have a helper function that reads an
  ;; equation in a natural language and turns that into an
  ;; s-expression
  ;; possibly in a human-readable form
  ;;   for y=-2x^2+8
  ;;   to
  ;;   #(-> #(* % %) (* -2) (+ 8))
  ;;
  ;; also, for the quadratic functions (and for linear functions as well)
  ;; the estimation depends on the fact whether the function increases or
  ;; decreases based on the "a" factor (that multiplies the "x" itself)
  ;; I guess the rule of thumb is that for every negative "a" factor,
  ;; LRS overestimates it
  ;; TODO: to be researched
  ;; "The left Riemann sum underestimates the area under the
  ;; respective curve if the function is increasing over the given interval."


  (def f #(-> (* % %) (* -2) (+ 8)))
  (left-riemann-sum f [0 1] 0.25)
  ;; => 7.5625

  (def f #(- (* 3 (* % %))
             (* % % %)))
  (left-riemann-sum f [0.5 2.5] 0.5)
  )

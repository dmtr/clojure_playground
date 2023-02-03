(ns clojure-playground.perceptron)


(defn perceptron
  [num-features]
  {:num-features num-features :weights (vec
                                         (repeat num-features 0.0)) :bias 0.0})


(defn forward
  [perceptron x]
  (let [t (map vector x (:weights perceptron))
        weighted-sum-z (reduce #(+ %1 (* (first %2) (second %2))) (:bias perceptron) t)]
    (if (> weighted-sum-z 0.0) 1 0)))


(defn update-coeffs
  [perceptron, x, y]
  (let [prediction (forward perceptron x)
        err (- y prediction)
        errors (map #(* err %) x)
        weights (map + (:weights perceptron) errors)]
    {:num-features (:num-features perceptron) :weights (vec weights) :bias (+ (:bias perceptron) err) :err err}))


(defn train-perceptron
  [perceptron all-x all-y]
  (loop [p perceptron x all-x y all-y]
    (if (empty? x)
      p
      (let [[current-x & tail-x] x
            [current-y & tail-y] y
            current-p (update-coeffs p current-x current-y)]
        (recur current-p tail-x tail-y)))))

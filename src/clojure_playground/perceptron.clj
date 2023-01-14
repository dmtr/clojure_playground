(ns clojure-playground.perceptron
  (:require
    [clojure.string :as str]))


(def test-data-file "./data/test_data.txt")


(defn load-data
  [path]
  (map
    #(str/split % #"\t")
    (str/split-lines (slurp path))))


(defn parse-float
  [x]
  (Float/parseFloat x))


(defn get-data
  [path]
  (map #(map parse-float
             %) (drop 1
                      (load-data path))))


(def data (get-data test-data-file))


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
            current-p (update-coeffs p current-x (first current-y))]
        (recur current-p tail-x tail-y)))))


(defn train
  [model train-model-f all-x all-y epochs]
  (loop [n 0 res model]
    (println [n res])
    (if (= n epochs)
      res
      (let [updated (train-model-f res all-x all-y)]
        (recur (inc n) updated)))))


(def all-x (map #(vector (first %) (second %)) data))

(def all-y (map #(vector (last %)) data))

(def trained (train (perceptron 2) train-perceptron all-x all-y 5))

(forward trained [1.33,  2.03])

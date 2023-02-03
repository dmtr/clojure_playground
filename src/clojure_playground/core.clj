(ns clojure-playground.core
  (:require
    [clojure-playground.perceptron :refer [perceptron forward train-perceptron]]
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


(defn parse-int
  [x]
  (Integer/parseInt x))


(defn get-data
  [path]
  (map (fn [[x1 x2 y]] (vector (parse-float x1) (parse-float x2) (parse-int y)))
       (drop 1
             (load-data path))))


(def data (get-data test-data-file))


(defn train
  [model train-model-f all-x all-y epochs]
  (loop [n 0 res model]
    (println [n res])
    (if (= n epochs)
      res
      (let [updated (train-model-f res all-x all-y)]
        (recur (inc n) updated)))))


(def all-x (map #(vector (first %) (second %)) data))

(def all-y (map #(last %) data))

(def trained (train (perceptron 2) train-perceptron all-x all-y 5))

(forward trained [1.33,  2.03])


;; evaluate the results
(defn compute-accuracy
  [model all-x all-y]
  (let [predictions (map #(forward model %) all-x)
        correct (reduce + (map (fn [prediction y] (if (= prediction y) 1 0)) predictions all-y))]
    (/ correct (count all-y))))


(compute-accuracy trained all-x all-y)

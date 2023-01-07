(ns clojure-playground.perceptron
  (:require
    [clojure.string :as str]))


(def test-data-file "./data/test_data.txt")


(defn load-data
  [path]
  (map #(str/split % #"\t") (str/split-lines (slurp path))))


(defn parse-float
  [x]
  (Float/parseFloat x))


(defn get-data
  [path]
  (map #(map parse-float %) (drop 1 (load-data path))))


(defn perceptron
  [num-features]
  {:num-features num-features :weights (vec (repeat num-features 0.0)) :bias 0.0})


(defn with-indexes
  [coll]
  (loop [x 0 remain coll res []]
    (if (empty? remain)
      res
      (let [[e & remaining] remain] (recur (inc x) remaining (into res [[x, e]]))))))

(ns clojure-playground.core
  (:gen-class)
  (:require
    [clojure-playground.perceptron :refer [perceptron forward train-perceptron]]
    [clojure.string :as str]
    [clojure.tools.cli :refer [parse-opts]]))


(def test-data-file "./data/test_data.txt")


(def cli-options
  [["-f" "--file FILE" "Path to data file"
    :default test-data-file
    :parse-fn #(if (str/blank? %) test-data-file %)
    :validate [#(not (str/blank? %)) "File path cannot be blank"]]
   ["-e" "--epochs EPOCHS" "Number of epochs"
    :default 5
    :parse-fn #(Integer/parseInt %)
    :validate [#(>= % 1) "Number of epochs must be greater than 0"]]
   ["-h" "--help"]])


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


(defn train
  [model train-model-f all-x all-y epochs]
  (loop [n 0 res model]
    (println ["epoch" n "model" res])
    (if (= n epochs)
      res
      (let [updated (train-model-f res all-x all-y)]
        (recur (inc n) updated)))))


;; evaluate the results
(defn compute-accuracy
  [model all-x all-y]
  (let [predictions (map #(forward model %) all-x)
        correct (reduce + (map (fn [prediction y] (if (= prediction y) 1 0)) predictions all-y))]
    (/ correct (count all-y))))


(defn -main
  [& args]
  (let [options (:options (parse-opts args cli-options))
        data (get-data (:file options))
        all-x (map #(vector (first %) (second %)) data)
        all-y (map #(last %) data)
        trained (train (perceptron 2) train-perceptron all-x all-y (:epochs options))]
    (println "Accuracy: " (compute-accuracy trained all-x all-y))))

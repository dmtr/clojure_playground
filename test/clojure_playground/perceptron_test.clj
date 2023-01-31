(ns clojure-playground.perceptron-test
  (:require
    [clojure-playground.perceptron :refer [perceptron forward update-coeffs]]
    [clojure.test :refer [deftest is testing]]))


(deftest forward-test
  (testing "forward function"
    (is (= 0 (forward (perceptron 2) [1 2])))))


(deftest update-coeffs-test
  (testing "update-coeffs function"
    (let [p (update-coeffs (perceptron 2) [1 2] 1)]
      (is (= [1.0 2.0] (:weights p)))
      (is (= 1 (:err p)))
      (is (= 1.0 (:bias p))))))

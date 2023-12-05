(ns year-2016.day1-test
  (:require [clojure.test :refer [deftest is testing]]
            [year-2016.day1 :refer [get-new-direction step get-movement-coords]]))

(deftest get-new-direction-test
  (testing "get-new-direction")
  (is (= "EAST" (get-new-direction "NORTH" \R)))
  (is (= "WEST" (get-new-direction "NORTH" \L)))
  (is (= "NORTH" (get-new-direction "WEST" \R)))
  (is (= "SOUTH" (get-new-direction "WEST" \L)))
  (is (= "WEST" (get-new-direction "SOUTH" \R)))
  (is (= "EAST" (get-new-direction "SOUTH" \L)))
  (is (= "NORTH" (get-new-direction "EAST" \L)))
  (is (= "SOUTH" (get-new-direction "EAST" \R))))

(deftest get-movement-coords-test
  (testing "get-movement-coords")
  (is (= '(0 3) (get-movement-coords "NORTH" 3))))

(deftest step-test
  (testing "move")
  (is (= {:coords '(-3 0) :direction "WEST"} (step {:coords '(0 0) :direction "NORTH"} \L 3))))
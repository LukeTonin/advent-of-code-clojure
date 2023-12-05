(ns year-2015.day2
  (:require [clojure.string :as str]))

;; Problem 1

(defn split [string] (str/split string #"x"))

(defn get-two-smallest [lst]
  (take 2 (sort lst)))

(defn get-surface [lst]
  (+ (* 2 (nth lst 0) (nth lst 1))
     (* 2 (nth lst 1) (nth lst 2))
     (* 2 (nth lst 0) (nth lst 2))))

(defn get-extra [lst]
  (apply * (get-two-smallest lst)))


(defn get-total [lst]
  (+ (get-surface lst) (get-extra lst)))

(defn split-and-cast [line]
  (->> line split (map #(Integer/parseInt %))))

(defn process-line [line]
  (->> line split-and-cast
       get-total))

(->> "data/2015/day2.txt" slurp clojure.string/split-lines
     (map process-line)
     (reduce +))

;; Problem 2

(defn get-ribbon-1 [lst]
  (* 2 (apply + (get-two-smallest lst))))

(defn get-ribbon-2 [lst]
  (apply * lst))

(defn get-ribbon [lst]
  (+ (get-ribbon-1 lst) (get-ribbon-2 lst)))

(->> "data/2015/day2.txt" slurp clojure.string/split-lines (map split-and-cast)
     (map get-ribbon)
     (reduce +))


(ns year-2023.day4
  (:require [clojure.string :refer [split-lines split trim]]
            [clojure.set :refer [intersection]]
            [clojure.math :refer [pow]]))

(def test-string (slurp "data/2023/day4_test.txt"))
(def string (slurp "data/2023/day4.txt"))

;; Problem 1

(comment
  (parse-input test-string)
  (parse-input string)

  (println test-string)
  (println string))

(defn parse-card-input [string]
  (let [groups (rest (re-find #"Card\s+(\d+): ([\d\s]*) \| ([\d\s]*)" string))]
    {:id (Integer/parseInt (nth groups 0))
     :winning-nums (set (map #(Integer/parseInt %) (split (trim (nth groups 1)) #"\s+")))
     :nums (set (map #(Integer/parseInt %) (split (trim (nth groups 2)) #"\s+")))}))

(defn parse-input [string]
  (->> string split-lines (map parse-card-input)))

(defn get-num-winning [card-data]
  (count (intersection (:winning-nums card-data) (:nums card-data))))

(defn calculate-points [card-data]
  (let [num-winning (get-num-winning card-data)]
    (if (> num-winning 0) (int (pow 2 (- num-winning 1))) 0)))

#_(-> "Card 100: 41 48 83 86 17    | 83 86     6 31 17  9 48 53   " parse-card-input calculate-points)
#_(reduce + (->> test-string parse-input (map calculate-points)))

;; Result:
(assert (= (reduce + (->> string parse-input (map calculate-points))) 25651))

;; Problem 2

(def cards (parse-input string))
#_(def test-cards (parse-input test-string))

(defn assoc-increment-transient [v index inc-by]
  (assoc! v index (+ inc-by (nth v index))))

(defn add-next-cards-transient [card counts]
  (let [num-winning (get-num-winning card)
        index (:id card)]
    (loop [i 0 counts counts index index]
      (if (>= i num-winning) counts
          (recur (inc i)
                 (assoc-increment-transient counts (+ index i) (inc (nth counts (dec index))))
                 index)))))

(defn count-cards-transient
  ([cards]
   (count-cards-transient cards (transient (vec (repeat (count cards) 0)))))
  ([cards counts]
   (loop [cards cards counts counts]
     (if (= (count cards) 0)
       (persistent! counts)
       (recur (rest cards)
              (add-next-cards-transient (first cards) counts))))))

;; Result:
(assert (= (+ (reduce + (count-cards-transient cards)) (count cards)) 19499881))
(clojure.core/time (+ (reduce + (count-cards-transient cards)) (count cards)))


;; The following implementation does not use a transient vector. I thought this would result in slower performance,
;; but that doesn't seem to be the case.

(defn assoc-increment [v index inc-by]
  (assoc v index (+ inc-by (nth v index))))

(defn add-next-cards [card counts]
  (let [num-winning (get-num-winning card)
        index (:id card)]
    (loop [i 0 counts counts index index]
      (if (>= i num-winning) counts
          (recur (inc i)
                 (assoc-increment counts (+ index i) (inc (nth counts (dec index))))
                 index)))))

(defn count-cards
  ([cards]
   (count-cards cards (vec (repeat (count cards) 0))))
  ([cards counts]
   (loop [cards cards counts counts]
     (if (= (count cards) 0)
       counts
       (recur (rest cards)
              (add-next-cards (first cards) counts))))))

;; Result:
(assert (= (+ (reduce + (count-cards cards)) (count cards)) 19499881))
(clojure.core/time (+ (reduce + (count-cards cards)) (count cards)))

(ns year-2016.day1
  (:require clojure.string))

(defn get-new-direction
  [current-direction left-or-right]
  (let [directions '("NORTH" "EAST" "SOUTH" "WEST")
        current-index (.indexOf directions current-direction)
        new-index (cond (= \L left-or-right) (- current-index 1)
                        (= \R left-or-right) (+ current-index 1)
                        :else current-index)]
    (nth directions (mod new-index (count directions)))))

(defn get-movement-coords
  [direction distance]
  (case direction
    "NORTH" (list 0 (* distance 1))
    "SOUTH" (list 0 (* distance -1))
    "EAST" (list (* distance 1) 0)
    "WEST" (list (* distance -1) 0)))

(defn step
  [position left-or-right distance]
  (let [new-direction (get-new-direction (:direction position) left-or-right)
        new-coords (map +
                        (:coords position)
                        (get-movement-coords new-direction distance))]
    {:coords new-coords :direction new-direction}))

(defn parse-instruction
  [string]
  {:left-or-right (first string)
   :distance (Integer/parseInt (.substring string 1 (count string)))})

(defn input-to-instructions
  [string]
  (as-> string $
    (clojure.string/split $ #", ")
    (map (comp parse-instruction clojure.string/trim) $)))

(defn move
  [position instructions]
  (loop [position position
         instructions instructions] ;; this works just like a 'let'
                                    ;; binding.
    (if (= (count instructions) 0) ;; this is the base case.
      position
      (recur (step position
                   (:left-or-right (first instructions))
                   (:distance (first instructions)))
             (rest instructions)))))

(move {:coords '(0 0) :direction "NORTH"}
      '({:left-or-right \L :distance 10} {:left-or-right \R :distance 5}))

(defn solution_1
  "Solution to problem 1"
  [string]
  (->> string
       input-to-instructions
       (move {:coords '(0 0) :direction "NORTH"})))

(def string (slurp "data/2016/day1.txt"))
(solution_1 string)

;; Problem 2
(defn move-and-stop-on-return
  [position instructions]
  (loop [position position
         instructions instructions
         coords #{}] ;; this works just like a 'let' binding.
    (if (or (= (count instructions) 0) (contains? coords (:coords position))) 
      position
      (let [new-position (step position
                               (:left-or-right (first instructions))
                               (:distance (first instructions)))]
        (recur new-position
               (rest instructions)
               (conj coords (:coords position)))))))

(defn break-down-instruction
  [instruction]
  (if (= 1 (:distance instruction))
    instruction
    (conj (repeat (dec (:distance instruction))
                  {:left-or-right nil :distance 1})
          {:left-or-right (:left-or-right instruction) :distance 1})))

(defn solution_2
  "Solution to problem 2"
  [string]
  (->> string
       input-to-instructions
       (map break-down-instruction)
       flatten
       (move-and-stop-on-return {:coords '(0 0) :direction "NORTH"})))

#_(flatten (map break-down-instruction (input-to-instructions string)))

;; Result
(reduce + (map abs (:coords (solution_2 string))))

(reduce + (map abs (:coords (solution_2 "R8, R4, R4, R8"))))






(ns year-2015.day1)


(defn up-or-down [char]
  (if (= char \() 1 -1))


(defn process-line [line]
  (reduce + (map up-or-down line)))

(process-line "(()))")

(def line
  (-> "data/2015/day1.txt" slurp))

(process-line line)

(defn process-line-2 [line]
  (loop [char (first line) position 0 level 0] 
    (if (= level -1)
      position
      (recur (get line (inc position)) (inc position) (+ level (up-or-down char))))))

(process-line-2 "())))")
(process-line-2 line) 
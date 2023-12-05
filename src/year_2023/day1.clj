(ns year-2023.day1)

(defn filter-numeric-chars [string]
  (let [numeric-chars (set "0123456789")]
    (->> string
         (filter numeric-chars)
         (apply str))))

(defn process-line [line]
  (let [digits (filter-numeric-chars line)]
    (Integer/parseInt (str (first digits) (last digits)))))

(defn process-file [file-path]
  (let [lines (-> file-path slurp clojure.string/split-lines)]
    (reduce + (map process-line lines))))

(assert (= (process-file "data/2023/day1.txt") 54601))

(defn process-line-2 [line]
  (let [base-pattern #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))"
        list (->> line (re-seq base-pattern) (map second))
        num-map {:first (first list) :second (last list)}
        num-to-digit {"one" "1" "two" "2" "three" "3" "four" "4" "five" "5" "six" "6" "seven" "7" "eight" "8" "nine" "9"}
        num-map (assoc num-map
                       :first (if (contains? num-to-digit (:first num-map)) (num-to-digit (:first num-map)) (:first num-map))
                       :second (if (contains? num-to-digit (:second num-map)) (num-to-digit (:second num-map)) (:second num-map)))]
    (Integer/parseInt (str (:first num-map) (:second num-map)))))

(defn process-file-2 [file-path]
  (let [lines (-> file-path slurp clojure.string/split-lines)]
    (reduce + (map process-line-2 lines))))


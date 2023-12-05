(ns year-2023.day2)

;; Problem 1

(def available-cubes {:red 12 :green 13 :blue 14})

(def lines
  (->> "data/2023/day2.txt" slurp clojure.string/split-lines))

(defn parse-game [string]
  (as-> string $
    (clojure.string/split $ #";") (map (comp remove-nil parse-hand) $)))

(defn remove-nil [map]
  (into {} (filter (comp some? val) map)))

(defn parse-colour-factory [colour]
  (fn parse-colour [string]
    (some->> string
             (re-find (re-pattern (str "(\\d+) " colour)))
             second
             Integer/parseInt)))

(defn parse-hand [string]
  (let [parse-red (parse-colour-factory "red")
        parse-green (parse-colour-factory "green")
        parse-blue (parse-colour-factory "blue")]
    {:red (parse-red string)
     :green (parse-green string)
     :blue (parse-blue string)}))

(defn get-game-id [string]
  (->> string
       (re-find #"Game (\d+)")
       second
       Integer/parseInt))


(defn hand-possible? [parsed-hand available-cubes]
  (and (or (not (contains? parsed-hand :red)) (<= (:red parsed-hand) (:red available-cubes)))
       (or (not (contains? parsed-hand :green)) (<= (:green parsed-hand) (:green available-cubes)))
       (or (not (contains? parsed-hand :blue)) (<= (:blue parsed-hand) (:blue available-cubes)))))

(defn game-possible? [line available-cubes]
  (every? #(hand-possible? % available-cubes) (parse-game line)))

(game-possible? (nth lines 0) available-cubes)

(defn parse-games [lines] (let [games-possible (mapv game-possible? lines (repeat available-cubes))
                                game-ids (mapv get-game-id lines)]
                            (reduce + (mapv (fn [x y] (* (if x 1 0) y)) games-possible game-ids))))

(assert (= 2006 (parse-games lines)))

;; Problem 2

(defn fill-nil
  ([map-]
   (fill-nil 0 map-))
  ([value map-]
   (into {} (map (fn [[k v]] [k (if (nil? v) value v)]) map-))))

(defn parse-game-2 [string]
  (as-> string $
    (clojure.string/split $ #";") (map (comp fill-nil parse-hand) $)))


((comp fill-nil parse-hand) " red 3 green 4 blue")

(fill-nil (parse-hand " red 3 green 4 blue"))

(parse-game-2 (nth lines 0))

(defn get-max-map [string]
  (let [parsed-game (parse-game-2 string)]
    {:red (apply max (map :red parsed-game))
     :green (apply max (map :green parsed-game))
     :blue (apply max (map :blue parsed-game))}))

(let [parsed-game (parse-game-2 (nth lines 0))]
  (apply max (map :red parsed-game)))

(defn get-power [map-] (reduce * (vals map-)))

(get-power (get-max-map (nth lines 0)))

(reduce + (map (comp get-power get-max-map) lines))





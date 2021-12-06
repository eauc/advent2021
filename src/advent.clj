(ns advent
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 01
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day01-file (io/resource "day01.txt"))

(def day01-data
  (->> (line-seq (io/reader day01-file))
       (map edn/read-string)))

(defn count-increment [xs]
  (->> xs
       (partition 2 1)
       (map (fn [[x y ]] (if (< x y) :increased :decreased)))
       (filter #{:increased})
       count))

(count-increment day01-data)
;; => 1532

(->> day01-data
     (partition 3 1)
     (map #(apply + %))
     count-increment)
;; => 1571

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day02-file (io/resource "day02.txt"))

(def day02-data
  (->> (line-seq (io/reader day02-file))
       (map #(clojure.string/split % #"\s"))
       (map (fn [[x y ]] [(keyword x) (edn/read-string y)]))))

(def answer-01
  (let [{:keys [forward down up]} (->> day02-data
                                       (group-by first)
                                       (map (fn [[d ms]] [d (apply + (map second ms))]))
                                       (into {}))
        x forward
        depth (- down up)]
    (* x depth)))
;; => 1714680

(defn move [state [cmd n]]
  (let [{:keys [x depth aim]} state]
    (case cmd
      :forward (-> state (update :x + n) (update :depth + (* aim n)))
      :down (update state :aim + n)
      :up (update state :aim - n))))

(def answer-02
  (let [{:keys [x depth]} (reduce move {:x 0 :depth 0 :aim 0} day02-data)]
    (* x depth)))
;; => 1963088820

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 03
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day03-file (io/resource "day03.txt"))

(def day03-data
  (->> (line-seq (io/reader day03-file))))

(let [gamma (->> (first day03-data)
                 (map-indexed (fn [i _] (mapv #(str (get % i)) day03-data)))
                 (map frequencies)
                 (map (fn [{ones "1" zeros "0"}] (if (> ones zeros) 1 0)))
                 reverse
                 (map-indexed (fn [p bit] [bit (int (Math/pow 2 p))]))
                 (filter #(= 1 (first %)))
                 (map second)
                 (reduce +))
      epsilon (- (dec (int (Math/pow 2 (count (first day03-data))))) gamma)
      ]
  {:gamma gamma
   :epsilon epsilon
   :power-consumption (* gamma epsilon)})
;; => {:gamma 3516, :epsilon 579, :power-consumption 2035764}

(defn get-rating [all-data pick-matching-bit]
  (-> (loop [data all-data
             n 0]
        (let [matching-bit (->> data
                                (map #(get % n))
                                (frequencies)
                                (sort-by (juxt second first))
                                pick-matching-bit
                                first)
              valid-data (filter #(= matching-bit (get % n)) data)]
          (println valid-data)
          (if (= 1 (count valid-data))
            (first valid-data)
            (recur valid-data (inc n)))))
      (Integer/parseInt 2)))

(def test-data
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])

(def test-ratings
  (let [o2-generator (get-rating test-data last)
        co2-scrubber (get-rating test-data first)]
    {:o2-generator o2-generator
     :co2-scrubber co2-scrubber
     :life-support (* o2-generator co2-scrubber)}))
;; => {:o2-generator 23, :co2-scrubber 10, :life-support 230}

(def ratings
  (let [o2-generator (get-rating day03-data last)
        co2-scrubber (get-rating day03-data first)]
    {:o2-generator o2-generator
     :co2-scrubber co2-scrubber
     :life-support (* o2-generator co2-scrubber)}))
;; => {:o2-generator 3311, :co2-scrubber 851, :life-support 2817661}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 04
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day04-file (io/resource "day04.txt"))

(def day04-data
  (->> (line-seq (io/reader day04-file))))

(def bingo-numbers
  (->> (clojure.string/split (first day04-data) #",")
       (map #(Integer/parseInt %))))

(defn txt->line [line]
  (->> (-> line clojure.string/trim (clojure.string/split #"\s+"))
       (mapv #(Integer/parseInt %))))

(defn txt->board [lines]
  (let [rows (mapv txt->line lines)]
    {:rows rows
     :cols (apply map vector rows)}))

(def bingo-boards
  (->> day04-data
       (drop 2)
       (partition 5 6)
       (map txt->board)))


(defn play-number-on-grid [grid n]
  (mapv #(remove #{n} %) grid))

(defn play-number-on-board [{:keys [rows cols]} n]
  {:rows (play-number-on-grid rows n)
   :cols (play-number-on-grid cols n)})

(defn grid-winning? [grid]
  (some empty? grid))

(defn board-winning? [{:keys [rows cols]}]
  (or (grid-winning? rows)
      (grid-winning? cols)))

(comment
  (-> (first bingo-boards)
      (play-number-on-board 79)
      (play-number-on-board 81)
      (play-number-on-board 40)
      (play-number-on-board 28)
      (play-number-on-board 77)
      board-winning?)
  )

(defn board-score [{:keys [rows]}]
  (reduce #(+ %1 (reduce + 0 %2)) 0 rows))

(defn find-first-winning-board [start-boards start-numbers]
  (loop [boards start-boards
         numbers start-numbers]
    (let [new-boards (mapv #(play-number-on-board % (first numbers)) boards)
          winning-board (first (filter board-winning? new-boards))]
      (if winning-board
        [winning-board (first numbers)]
        (recur new-boards (rest numbers))))))

(let [[first-winning-board number] (find-first-winning-board bingo-boards bingo-numbers)]
  (* number (board-score first-winning-board)))
;; => 33462

(defn last-first-winning-board [start-boards start-numbers]
  (loop [boards start-boards
         numbers start-numbers]
    (let [new-boards (mapv #(play-number-on-board % (first numbers)) boards)
          losing-boards (remove board-winning? new-boards)]
      (if (empty? losing-boards)
        [(first new-boards) (first numbers)]
        (recur losing-boards (rest numbers))))))

(let [[last-winning-board number] (last-first-winning-board bingo-boards bingo-numbers)]
  (* number (board-score last-winning-board)))
;; => 30070


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 05
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day05-file (io/resource "day05.txt"))

(def day05-data
  (->> (line-seq (io/reader day05-file))
       (map #(clojure.string/split % #"(,| -> )"))
       (map (fn [strs] (map #(Integer/parseInt %) strs)))))

(defn line-positions [[x1 y1 x2 y2]]
  (let [rng-x (if (>= x1 x2) (range x1 (dec x2) -1) (range x1 (inc x2)))
        rng-y (if (>= y1 y2) (range y1 (dec y2) -1) (range y1 (inc y2)))]
    (cond
      (= x1 x2) (set (map #(vector x1 %) rng-y))
      (= y1 y2) (set (map #(vector % y1) rng-x))
      :else (set (map vector rng-x rng-y)))))

(defn horizontal? [[x1 y1 x2 y2]]
  (= y1 y2))

(defn vertical? [[x1 y1 x2 y2]]
  (= x1 x2))

(defn intersections-count [lines]
  (->> lines
       (mapcat line-positions)
       frequencies
       (filter #(< 1 (second %)))
       count))

(comment
  (->> [[0,9,5,9]
        [8,0,0,8]
        [9,4,3,4]
        [2,2,2,1]
        [7,0,7,4]
        [6,4,2,0]
        [0,9,2,9]
        [3,4,1,4]
        [0,0,8,8]
        [5,5,8,2]]
       ;; (filter #(or (horizontal? %) (vertical? %)))
       intersections-count)
  )

(->> day05-data
     (filter #(or (horizontal? %) (vertical? %)))
     intersections-count)
;; => 4745

(->> day05-data
     intersections-count)
;; => 18442

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 06
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day06-file (io/resource "day06.txt"))

(def day06-data
  (->> (line-seq (io/reader day06-file))
       first
       (#(clojure.string/split % #","))
       (map #(Integer/parseInt %))))

(def test-data
  [3,4,3,1,2])

(defn one-day-pass [fishes]
  (let [new-fishes (map (fn [x _] x) (repeat 8) (filter #{0} fishes))]
    (concat (map #(if (= 0 %) 6 (dec %)) fishes) new-fishes)))

(defn x-days-pass [fishes x]
  (reduce (fn [fs _] (one-day-pass fs)) fishes (range x)))

(count (x-days-pass test-data 80))
;; => 5934

(count (x-days-pass day06-data 80))
;; => 352151

(def spawned
  (memoize
   (fn [start total-days]
     (let [spawn-days (range (+ start 7) (inc total-days) 7)]
       (+ (count spawn-days)
          (reduce + (map #(spawned (+ % 2) total-days) spawn-days)))))))

(spawned (- (inc 3) 7) 18)

(defn n-fishes [start-fishes total-days]
  (+ (count start-fishes)
     (reduce + (map #(spawned (- (inc %) 7) total-days) start-fishes))))

(n-fishes test-data 18)
;; => 26
(n-fishes test-data 80)
;; => 5934
(n-fishes day06-data 80)
;; => 352151
(n-fishes day06-data 256)
;; => 1601616884019

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

(->> day01-data
     (partition 3 1)
     (map #(apply + %))
     count-increment)

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

(defn run [opts]
  (println "coucouc"))

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

(def ratings
  (let [o2-generator (get-rating day03-data last)
        co2-scrubber (get-rating day03-data first)]
    {:o2-generator o2-generator
     :co2-scrubber co2-scrubber
     :life-support (* o2-generator co2-scrubber)}))

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
     :cols (map-indexed (fn [i _] (mapv #(nth % i) rows)) rows)}))

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
      ;; (play-number-on-board 28)
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

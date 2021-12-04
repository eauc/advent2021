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

(map-indexed (fn [i p] [i p]) (first day03-data))

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

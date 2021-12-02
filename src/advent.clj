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

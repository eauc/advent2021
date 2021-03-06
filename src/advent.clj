(ns advent
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn sum [xs]
  (reduce + 0 xs))

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
                 sum)
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
  (reduce #(+ %1 (sum %2)) 0 rows))

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
          (sum (map #(spawned (+ % 2) total-days) spawn-days)))))))

(spawned (- (inc 3) 7) 18)

(defn n-fishes [start-fishes total-days]
  (+ (count start-fishes)
     (sum (map #(spawned (- (inc %) 7) total-days) start-fishes))))

(n-fishes test-data 18)
;; => 26
(n-fishes test-data 80)
;; => 5934
(n-fishes day06-data 80)
;; => 352151
(n-fishes day06-data 256)
;; => 1601616884019

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 07
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day07-file (io/resource "day07.txt"))

(def day07-data
  (map #(Integer/parseInt %)
       (clojure.string/split (first (line-seq (io/reader day07-file))) #",")))

(defn move-fuel-cost-simple [dist]
  dist)

(defn pos-fuel-cost [pos move-fuel-cost]
  (sum (map #(move-fuel-cost (Math/abs (- % pos))) day07-data)))

(apply min (map #(pos-fuel-cost % move-fuel-cost-simple) day07-data))
;; => 354129

(def move-fuel-cost-real
  (memoize
   (fn [dist]
     (/ (* dist (inc dist)) 2))))

(def move-fuel-cost-real-bourrin
  (memoize
   (fn [dist]
     (sum (range (inc dist))))))

(apply min (map #(pos-fuel-cost % move-fuel-cost-real) day07-data))
;; => 98905973
(apply min (map #(pos-fuel-cost % move-fuel-cost-real-bourrin) day07-data))
;; => 98905973

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 08
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day08-file (io/resource "day08.txt"))

(def day08-data
  (->> (line-seq (io/reader day08-file))
       (map
        (fn [l]
          (let [[test output] (clojure.string/split l #" \| ")]
            {:test (->> (clojure.string/split test #" ") (map set))
             :output (clojure.string/split output #" ")})))))

(defn uniq-digit [d]
  (#{7 4 3 2} (count d)))

(->> day08-data
     (mapcat :output)
     (filter uniq-digit)
     count)
;; => 543

(def test-data
  (->> ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
       "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
       "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
       "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
       "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
       "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
       "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
       "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
       "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
       "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"]
       (map (fn [l]
              (let [[test output] (clojure.string/split l #" \| ")]
                {:test (->> (clojure.string/split test #" ") (map set))
                 :output (clojure.string/split output #" ")})))))

(defn common-segments [by-seg-count counts]
  (apply clojure.set/intersection
         (mapcat #(get by-seg-count %) counts)))

(defn decode [test]
  (let [by-seg-count (group-by count test)
        f (common-segments by-seg-count [2 3 4 6 7])
        a (common-segments by-seg-count [3 5 6 7])
        d (common-segments by-seg-count [4 5 7])
        b (clojure.set/difference
           (common-segments by-seg-count [4 6 7])
           f)
        c (clojure.set/difference
           (common-segments by-seg-count [2 3 4 7])
           f)
        g (clojure.set/difference
           (common-segments by-seg-count [5 6 7])
           a)
        e (clojure.set/difference
           #{\a \b \c \d \e \f \g}
           (clojure.set/union a b c d f g))
        ]
    (into {}
          [[(first a) \a] [(first b) \b] [(first c) \c] [(first d) \d]
           [(first e) \e] [(first f) \f] [(first g) \g]])))

(def seg->digit
  {[\a \b \c \e \f \g] "0"
   [\c \f] "1"
   [\a \c \d \e \g] "2"
   [\a \c \d \f \g] "3"
   [\b \c \d \f] "4"
   [\a \b \d \f \g] "5"
   [\a \b \d \e \f \g] "6"
   [\a \c \f] "7"
   [\a \b \c \d \e \f \g] "8"
   [\a \b \c \d \f \g] "9"})

(defn decode-output [output code]
  (Integer/parseInt
   (clojure.string/join
    (map #(get seg->digit (sort (map code %))) output))))

(defn decode-line [{:keys [test output]}]
  (let [code (decode test)]
    (decode-output output code)))

(sum
 (map decode-line test-data))
;; => 61229

(sum
 (map decode-line day08-data))
;; => 994266

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 09
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day09-file (io/resource "day09.txt"))

(defn ->map [lines]
  (->> lines
       (map #(clojure.string/split % #""))
       (map (fn [l] (map #(Integer/parseInt %) l)))))

(def day09-data
  (->map (line-seq (io/reader day09-file))))

(def test-data
  (->map
   ["2199943210"
    "3987894921"
    "9856789892"
    "8767896789"
    "9899965678"]))

(defn height [m [x y]]
  (-> m (nth y) (nth x)))

(defn size [m]
  [(count (first m)) (count m)])

(defn ->all-positions [m]
  (let [[w h] (size m)]
    (for [x (range w)
          y (range h)]
      [x y])))

(def neighbours
  (memoize
   (fn [[x y] [w h]]
     (cond-> (list)
       (< 0 x) (conj [(dec x) y])
       (< 0 y) (conj [x (dec y)])
       (< x (dec w)) (conj [(inc x) y])
       (< y (dec h)) (conj [x (inc y)])))))

(neighbours [2 0] (size test-data))

(defn ->local-minimums [m]
  (let [s (size m)
        positions (->all-positions m)]
    (filter
     (fn [p]
       (let [ph (height m p)
             ns (neighbours p s)]
         (every? #(< ph (height m %)) ns)))
     positions)))

(defn risk-level [m p]
  (inc (height m p)))

(defn risk-assessment [m]
  (->> (->local-minimums m)
       (map #(risk-level m %))
       sum))

(risk-assessment test-data)
(risk-assessment day09-data)
;; => 502

(defn map->string
  ([m f]
   (clojure.string/join
    "\n"
    (map-indexed
     (fn [y row]
       (clojure.string/join
        (map-indexed
         (fn [x ph]
           (let [v (f [x y] ph)]
             (if-not (nil? v) v \.)))
         row)))
     m)))
  ([m]
   (map->string m (fn [_ ph] ph))))

(do
  (println "===================")
  (println
   (map->string test-data)))

(do
  (println "===================")
  (println
   (map->string test-data (fn [_ ph] (when (= 9 ph) ph)))))

(let [mins (set (->local-minimums test-data))]
  (println "===================")
  (println
   (map->string test-data (fn [p ph] (when (mins p) ph)))))

(do
  (println "===================")
  (println
   (map->string day09-data)))

(do
  (println "===================")
  (println
   (map->string day09-data (fn [_ ph] (when (= 9 ph) ph)))))

(let [mins (set (->local-minimums day09-data))]
  (println "===================")
  (println
   (map->string day09-data (fn [p ph] (when (or (= 9 ph) (mins p)) ph)))))

(defn up-hill-neighbours [m s p]
  (let [ph (height m p)]
    (filter
     (fn [n]
       (let [nh (height m n)]
         (and (not= 9 nh) (>= nh ph))))
     (neighbours p s))))

(defn ->bassin [m p]
  (let [s (size m)]
    (loop [ps #{p}
           result #{}]
      (if (empty? ps)
        result
        (let [n (first ps)
              up-hill-ns (up-hill-neighbours m s n)
              new-result (conj result n)]
          (recur
           (clojure.set/difference
            (apply conj ps up-hill-ns)
            new-result)
           new-result))))))

(defn ->bassins [m]
  (let [mins (->local-minimums m)]
    (map #(conj (->bassin m %) %) mins)))

(let [ps (apply clojure.set/union (->bassins test-data))]
  (println "===================")
  (println ps)
  (println
   (map->string test-data (fn [p ph] (when (ps p) ph)))))

(let [ps (apply clojure.set/union (->bassins day09-data))]
  (println "===================")
  ;; (println ps)
  (println
   (map->string day09-data (fn [p ph] (when (ps p) ph)))))

(->> (->bassins test-data)
     (map count)
     sort
     reverse
     (take 3)
     (reduce * 1))
;; => 1134

(->> (->bassins day09-data)
     (map count)
     sort
     reverse
     (take 3)
     (reduce * 1))
;; => 1330560

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day10-file (io/resource "day10.txt"))

(defn ->program [lines]
  (->> lines
       (map #(clojure.string/split % #""))))

(def day10-data
  (->program (line-seq (io/reader day10-file))))

(def test-data
  (->program
   ["[({(<(())[]>[[{[]{<()<>>"
    "[(()[<>])]({[<{<<[]>>("
    "{([(<{}[<>[]}>{[]{[(<()>"
    "(((({<>}<{<{<>}{[]{[]{}"
    "[[<[([]))<([[{}[[()]]]"
    "[{[{({}]{}}([{[{{{}}([]"
    "{<[[]]>}<{[{[{[]{()[[[]"
    "[<(<(<(<{}))><([]([]()"
    "<{([([[(<>()){}]>(<<{{"
    "<{([{{}}[<[[[<>{}]]]>[]]"]))

(def pairs
  {"(" ")"
   "[" "]"
   "{" "}"
   "<" ">"})

(defn parse [line]
  (loop [chars line
         state (list)]
    (if (empty? chars)
      {:state state}
      (let [ch (first chars)]
        (if (#{"(" "[" "{" "<"} ch)
          (recur (rest chars) (conj state ch))
          (let [last-state (first state)]
            (if (= ch (pairs last-state))
              (recur (rest chars) (pop state))
              {:syntax-error ch})))))))

(def syntax-error-score
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(defn total-syntax-error-score [lines]
  (->> lines
       (map parse)
       (map :syntax-error)
       (map syntax-error-score)
       (filter identity)
       sum))

(total-syntax-error-score test-data)
;; => 26397
(total-syntax-error-score day10-data)
;; => 215229

(defn complete-sequence [state]
  (map pairs state))

(def complete-score
  {")" 1
   "]" 2
   "}" 3
   ">" 4})

(defn complete-sequence-score [complete-sequence]
  (reduce
   (fn [score ch]
     (+ (* 5 score) (complete-score ch)))
   0
   complete-sequence))

(defn autocomplete-score [scores]
  (let [n (count scores)
        i (Math/floor (/ n 2))]
    (println i)
    (nth (sort scores) i)))

(defn total-autocomplete-score [lines]
  (->> lines
       (map parse)
       (remove :syntax-error)
       (map :state)
       (map complete-sequence)
       (map complete-sequence-score)
       auto-complete-score))

(total-autocomplete-score test-data)
;; => 288957
(total-autocomplete-score day10-data)
;; => 1105996483

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day11-file (io/resource "day11.txt"))

(defn ->octopuses [lines]
  (->> lines
       (map #(clojure.string/split % #""))
       (map (fn [l] (map #(Integer/parseInt %) l)))))

(def day11-data
  (->octopuses (line-seq (io/reader day11-file))))

(def test-data
  (->octopuses
   ["5483143223"
    "2745854711"
    "5264556173"
    "6141336146"
    "6357385478"
    "4167524645"
    "2176841721"
    "6882881134"
    "4846848554"
    "5283751526"]))

(defn oct->string
  ([m f]
   (clojure.string/join
    "\n"
    (map-indexed
     (fn [y l]
       (clojure.string/join
        (map-indexed
         (fn [x e]
           (let [v (f [x y] e)]
             (if (nil? v) "." (if (and (integer? v) (< 9 v)) "#" v))))
         l)))
     m)))
  ([m]
   (oct->string m (fn [_ e] e))))

(def adjacents
  (memoize
   (fn [[x y] [w h]]
     (set
      (cond-> (list)
        (and (< 0 x)
             (< 0 y)) (conj [(dec x) (dec y)])
        (and (< x (dec w))
             (< 0 y)) (conj [(inc x) (dec y)])
        (and (< 0 x)
             (< y (dec h))) (conj [(dec x) (inc y)])
        (and (< x (dec w))
             (< y (dec h))) (conj [(inc x) (inc y)])
        (< 0 x) (conj [(dec x) y])
        (< 0 y) (conj [x (dec y)])
        (< x (dec w)) (conj [(inc x) y])
        (< y (dec h)) (conj [x (inc y)]))))))

(let [adjs (set (adjacents [3 3] (size test-data)))]
  (println "========================")
  (println adjs)
  (println (oct->string test-data (fn [p _] (when (adjs p) "X")))))

(defn increase-energy
  ([o f]
   (map-indexed
    (fn [y l]
      (map-indexed
       (fn [x e]
         (if (f [x y] e) (inc e) e))
       l))
    o))
  ([o]
   (increase-energy o (fn [_ _] true))))

(defn flashing [o]
  (set
   (apply
    clojure.set/union
    (map-indexed
     (fn [y l]
       (filter
        identity
        (map-indexed
         (fn [x e]
           (when (< 9 e) [x y]))
         l)))
     o))))

(defn flash [o']
  (let [s (size o')]
    (loop [o o'
           fls #{}]
      (let [new-fls (clojure.set/difference (flashing o) fls)
            adjs (map #(adjacents % s) new-fls)
            adjs-u (apply clojure.set/union adjs)
            new-o (reduce (fn [mem as] (increase-energy mem (fn [p _] (as p)))) o adjs)]
        ;; (println "========================")
        ;; (println new-fls)
        ;; (println (oct->string new-o (fn [p e] (cond
        ;;                                         (new-fls p) "X"
        ;;                                         (adjs-u p) e
        ;;                                         :else nil))))
        (if (empty? new-fls)
          new-o
          (recur new-o (clojure.set/union fls new-fls))))))))

(defn reset [o]
  (map (fn [l] (map #(if (< 9 %) 0 %) l)) o))

(defn oct-step [{:keys [o n-flashes]}]
  (let [new-o (-> o increase-energy flash)]
    {:o (reset new-o)
     :n-flashes (+ n-flashes (count (flashing new-o)))}))

(defn run-oct [o n-steps]
(reduce
 (fn [mem _]
   (oct-step mem))
 {:o o :n-flashes 0}
 (range n-steps)))

(let [{:keys [o n-flashes]} (run-oct test-data 100)]
  (println "========================")
  (println n-flashes)
  (println (oct->string o (fn [_ e] (if (= 0 e) "." e))))
  n-flashes)
;; => 1656

(let [{:keys [o n-flashes]} (run-oct day11-data 100)]
  (println "========================")
  (println n-flashes)
  (println (oct->string o (fn [_ e] (if (= 0 e) "." e))))
  n-flashes)
;; => 1749

(defn first-sync [o']
  (loop [o o'
         n 1]
    (let [new-o (-> o increase-energy flash reset)]
      (if (every? (fn [l] (every? #(= 0 %) l)) new-o)
        n
        (recur new-o (inc n))))))

(first-sync test-data)
;; => 195
(first-sync day11-data)
;; => 285

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day12-file (io/resource "day12.txt"))

(defn ->caves-map [lines]
  (let [pairs (map #(clojure.string/split % #"-" ) lines)
        paths (concat pairs (map reverse pairs))]
    (->> (group-by first paths)
         (map (fn [[ k v ]] [k (set (map second v))]))
         (into {}))))

(def day12-data
  (->caves-map
   (line-seq (io/reader day12-file))))

(def test-data-A
  (->caves-map
   ["start-A"
    "start-b"
    "A-c"
    "A-b"
    "b-d"
    "A-end"
    "b-end"]))

(defn small-caves [caves-map]
  (->> caves-map
       keys
       (filter #(= (clojure.string/lower-case %) %))
       set)))

(small-caves test-data-A)

(defn forbidden?-one [small-caves previous-path]
  (set (filter small-caves previous-path)))

(defn paths-at
  [current-path caves-map forbidden?]
  (if (= "end" (last current-path))
    [current-path]
    (let [forbidden (forbidden? current-path)
          current-dests (caves-map (last current-path))
          possible-dests (clojure.set/difference current-dests forbidden)]
      ;; (println "=" current-path forbidden current-dests possible-dests)
      (if (empty? possible-dests)
        []
        (mapcat
         #(paths-at (conj current-path %) caves-map forbidden?)
         possible-dests)))))

(paths-at ["start" "A"] test-data-A
          (partial forbidden?-one (small-caves test-data-A)))
;; => (["start" "A" "b" "A" "end"] ["start" "A" "b" "A" "c" "A" "end"] ["start" "A" "b" "end"] ["start" "A" "end"] ["start" "A" "c" "A" "b" "A" "end"] ["start" "A" "c" "A" "b" "end"] ["start" "A" "c" "A" "end"])
(paths-at ["start" "A" "c"] test-data-A
          (partial forbidden?-one (small-caves test-data-A)))
;; => (["start" "A" "c" "A" "b" "A" "end"] ["start" "A" "c" "A" "b" "end"] ["start" "A" "c" "A" "end"])
(paths-at ["start" "A" "c" "A"] test-data-A
          (partial forbidden?-one (small-caves test-data-A)))
;; => (["start" "A" "c" "A" "b" "A" "end"] ["start" "A" "c" "A" "b" "end"] ["start" "A" "c" "A" "end"])
(paths-at ["start" "A" "c" "A" "b"] test-data-A
          (partial forbidden?-one (small-caves test-data-A)))
;; => (["start" "A" "c" "A" "b" "A" "end"] ["start" "A" "c" "A" "b" "end"])
(paths-at ["start" "A" "c" "A" "b" "d"] test-data-A
          (partial forbidden?-one (small-caves test-data-A)))
;; => []

(defn paths->string [paths]
  (clojure.string/join
   "\n"
   (sort
    (map #(clojure.string/join "," %)paths))))

(defn paths-one [caves-map]
  (paths-at ["start"] caves-map (partial forbidden?-one (small-caves caves-map))))

(defn forbidden?-two [small-caves previous-path]
  (let [previous-small-caves (filter small-caves previous-path)
        two-small-caves? (->> previous-small-caves
                              frequencies
                              (filter #(= 2 (second %)))
                              first)]
    ;; (println "->>" previous-path two-small-caves?)
    (if two-small-caves?
      (set previous-small-caves)
      #{"start"})))

(defn paths-two [caves-map]
  (paths-at ["start"] caves-map (partial forbidden?-two (small-caves caves-map))))

(let [paths (paths-one test-data-A)]
  (println "=============================")
  (println (paths->string paths))
  (count paths))
;; => 10

(def test-data-B
  (->caves-map
   ["dc-end"
    "HN-start"
    "start-kj"
    "dc-start"
    "dc-HN"
    "LN-dc"
    "HN-end"
    "kj-sa"
    "kj-HN"
    "kj-dc"]))

(let [paths (paths-one test-data-B)]
  (println "=============================")
  (println (paths->string paths))
  (count paths))
;; => 19

(def test-data-C
  (->caves-map
   ["fs-end"
    "he-DX"
    "fs-he"
    "start-DX"
    "pj-DX"
    "end-zg"
    "zg-sl"
    "zg-pj"
    "pj-he"
    "RW-he"
    "fs-DX"
    "pj-RW"
    "zg-RW"
    "start-pj"
    "he-WI"
    "zg-he"
    "pj-fs"
    "start-RW"]))

(let [paths (paths-one test-data-C)]
  (println "=============================")
  (println (paths->string paths))
  (count paths))
;; => 226

(let [paths (paths-one day12-data)]
  (println "=============================")
  (println (paths->string paths))
  (count paths))
;; => 4659

(let [paths (paths-two test-data-A)]
  (println "=============================")
  (println (paths->string paths))
  (count paths))
;; => 36

(let [paths (paths-two test-data-B)]
  (println "=============================")
  (println (paths->string paths))
  (count paths))
;; => 103

(let [paths (paths-two test-data-C)]
  (println "=============================")
  (println (paths->string paths))
  (count paths))
;; => 3509

(let [paths (paths-two day12-data)]
  (println "=============================")
  (count paths))
;; => 148962

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day13-file (io/resource "day13.txt"))

(def day13-data
(->infrared-manual
(line-seq (io/reader day13-file))))

(def test-data
  (->infrared-manual
   ["6,10"
    "0,14"
    "9,10"
    "0,3"
    "10,4"
    "4,11"
    "6,0"
    "6,12"
    "4,1"
    "0,13"
    "10,12"
    "3,4"
    "3,0"
    "8,4"
    "1,10"
    "2,14"
    "8,10"
    "9,0"
    ""
    "fold along y=7"
    "fold along x=5"]))

(defn ->positions [lines]
  (set
   (map
    #(mapv
      (fn [l]
        (Integer/parseInt l))
      (clojure.string/split % #","))
    lines)))

(defn ->instructions [lines]
  (map
   (fn [l]
     (let [[_ direction index] (re-find #"^fold along (.)=(\d+)$" l)]
       [(keyword direction) (Integer/parseInt index)]))
   lines))

(defn ->infrared-manual [lines]
  (let [[pos-lines _ instruction-lines] (partition-by empty? lines)]
    {:positions (->positions pos-lines)
     :instructions (->instructions instruction-lines)}))

(->infrared-manual test-data)

(defn ->size [positions]
  [(inc (apply max (map first positions)))
   (inc (apply max (map second positions)))])

(defn positions->string [positions]
  (let [[w h] (->size positions)]
    (clojure.string/join
     "\n"
     (map
      (fn [y]
        (clojure.string/join
         (map
          (fn [x]
            (if (positions [x y]) "X" " "))
          (range w))))
      (range h)))))

(do
  (println "=======================")
  (println (positions->string (:positions test-data))))

(do
(println "=======================")
(println (positions->string (:positions day13-data))))

(defn fold-coord [coord index]
  (if (< index coord) (- (* index 2) coord) coord))

(defn fold [[direction index] positions]
  (set
   (map
    (fn [[x y]]
      (case direction
        :x [(fold-coord x index) y]
        :y [x (fold-coord y index)]))
    positions)))

(let [ps (->> (:positions test-data)
              (fold [:y 7]))]
  (println "=======================")
  (println (positions->string ps)))

(let [ps (->> (:positions test-data)
              (fold [:y 7])
              (fold [:x 5]))]
  (println "=======================")
  (println (positions->string ps)))

(count
 (fold (first (:instructions test-data)) (:positions test-data)))
;; => 17

(count
 (fold (first (:instructions day13-data)) (:positions day13-data)))
;; => 671

(defn ->code [{:keys [positions instructions]}]
  (reduce
   (fn [res inst]
     (fold inst res))
   positions
   instructions))

(let [ps (->code test-data)]
  (println "=======================")
  (println (positions->string ps)))

(let [ps (->code day13-data)]
  (println "=======================")
  (println (positions->string ps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 14
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day14-file (io/resource "day14.txt"))

(def day14-data
  (->polymer-instructions
   (line-seq (io/reader day14-file))))

(def test-data
  (->polymer-instructions
   ["NNCB"
    ""
    "CH -> B"
    "HH -> N"
    "CB -> H"
    "NH -> C"
    "HB -> C"
    "HC -> B"
    "HN -> C"
    "NN -> C"
    "BH -> H"
    "NC -> B"
    "NB -> B"
    "BN -> B"
    "BB -> N"
    "BC -> B"
    "CC -> N"
    "CN -> C"]))

(defn ->polymer-instructions [lines]
  (let [template (first lines)
        rules (->> lines
                   (drop 2)
                   (map #(clojure.string/split % #" -> "))
                   (map (fn [[pair insertion]]
                          [pair
                           [(clojure.string/join [(first pair) insertion])
                            (clojure.string/join [insertion (last pair)])]]))
                   (into {}))]
    {:template template
     :rules rules}))

(defn poly-inc-freqs [freqs rules]
  (apply
   merge-with
   +
   (map
    (fn [[p c]]
      (let [[p1 p2] (rules p)]
        {p1 c p2 c}))
    freqs)))

(defn ->poly-freqs [{:keys [rules template]} n-cycles]
  (let [freqs (frequencies
               (map
                #(clojure.string/join %)
                (partition 2 1 template)))]
    (->> (range n-cycles)
         (reduce (fn [freqs _] (poly-inc-freqs freqs rules)) freqs)
         (map (fn [[k v]] {(first k) v}))
         (apply merge-with + {(last template) 1}))))


(->poly-freqs test-data 1)
;; => {\N 2, \C 2, \B 2, \H 1}
;; => "NCNBCHB"
(->poly-freqs test-data 2)
;; => {\N 2, \B 6, \C 4, \H 1}
;; => "NBCCNBBBCBHCB"
(->poly-freqs test-data 3)
;; => {\N 5, \B 11, \C 5, \H 4}
;; => "NBBBCNCCNBBNBNBBCHBHHBCHB"
(->poly-freqs test-data 4)
;; => {\N 11, \B 23, \C 10, \H 5}
;; => "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
(->poly-freqs test-data 5)
;; => {\N 23, \B 46, \C 15, \H 13}

(->poly-freqs test-data 10)
;; => {\B 1749, \C 298, \H 161, \N 865}

(defn result [freqs]
  (- (apply max (map second freqs))
     (apply min (map second freqs))))

(result
 (->poly-freqs test-data 10))
;; => 1588

(result
 (->poly-freqs test-data 40))
;; => 2188189693529

(result
 (->poly-freqs day14-data 10))
;; => 3587
(result
 (->poly-freqs day14-data 40))
;; => 3906445077999

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day15-file (io/resource "day15.txt"))

(def day15-data
  (->chitons-map
   (line-seq (io/reader day15-file))))

(def test-data
  (->chitons-map
   ["1163751742"
    "1381373672"
    "2136511328"
    "3694931569"
    "7463417111"
    "1319128137"
    "1359912421"
    "3125421639"
    "1293138521"
    "2311944581"]))

(defn ->chitons-map [lines]
   (map
    (fn [l]
      (map
       #(Integer/parseInt %)
       (clojure.string/split l #"")))
    lines))

(defn get-at [m [x y]]
  (nth (nth m y) x))

(defn neighbour-costs [[p c] m]
  (let [ns (neighbours p (size m))]
    (map (fn [p] [p (+ c (get-at m p))]) ns)))

(defn map->str [m f]
  (clojure.string/join
   "\n"
   (map-indexed
    (fn on-line [y line]
      (clojure.string/join
       (map-indexed
        (fn on-col [x r]
          (or (f [x y]) "."))
        line)))
    m)))

(defn update-costs [costs pcs]
  (reduce
   (fn [mem [p c]]
     (update mem p (fnil #(min % c) 1000000)))
   costs
   pcs))

(defn path [m n-cycles]
  (let [[w h] (size m)
        dest [(dec w) (dec h)]]
    (loop [costs {[0 0] 0}
           unresolved {[0 0] 0}
           n n-cycles]
      (if (get unresolved dest)
        [(count costs) (get unresolved dest)]
        (if (>= 0 n)
          (do (println "==============================")
              (println (map->str m #(when (get unresolved %) "X")))
              [(count costs) (get unresolved dest)])
          (let [min-cost (apply min (vals unresolved))
                to-resolve (filter #(= min-cost (second %)) unresolved)
                ns (mapcat #(neighbour-costs % m) to-resolve)
                new-costs (update-costs costs ns)
                new-unresolved (into
                                (reduce #(dissoc %1 (first %2)) unresolved to-resolve)
                                (filter (fn [[p c]] (< c (get costs p 1000000))) ns))]
            (recur new-costs new-unresolved (dec n))))))))


(do (println "============")
    (path test-data 40))
;; => 40


(do (println "============")
    (path day15-data 500))
;; => 447

(defn unfold-map [m]
  (let [first (map
               (fn [line]
                  (mapcat
                   (fn [i] (map #(inc (mod (dec (+ i %)) 9)) line))
                   (range 5)))
               m)]
    (mapcat
     (fn [i]
       (map
        (fn [line]
          (map #(inc (mod (dec (+ i %)) 9)) line))
        first))
     (range 5))))

(def new-test-data
  (unfold-map test-data))

(do (println "============")
    (path new-test-data 350))
;; => [2500 315]

(def new-day15-data
  (unfold-map day15-data))

(do (println "============")
    (path new-day15-data 3000))
;; => [250000 2825]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 16
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def day16-file (io/resource "day16.txt"))

(def day16-data
  (->bits
   (first
    (line-seq (io/reader day16-file)))))

(defn ->bits [s]
  (clojure.string/join
   (map
    (fn [c]
      (let [n (Integer/parseInt c 16)
            s (Integer/toString n 2)]
        (str
         (apply str (repeat (- 4 (count s)) "0"))
         s)))
    (clojure.string/split s #""))))

(->bits "0D1")

(def test-packet1
  "110100101111111000101000")

(defn align-bits [p-length]
  (if (= p-length (* 8 (quot p-length 8)))
    0
    (- 8 (rem p-length 8))))

(align-bits 21)
;; => 3

(defn ->literal-value-packet [pck cont]
  ;; (println :value)
  (loop [s cont
         v ""
         l (:p-length pck)]
    (let [new-v (str v (subs s 1 5))
          new-l (+ 5 l)
          new-s (subs s 5)]
      (if (= \0 (first s))
        [(assoc pck
                :value (BigInteger. new-v 2)
                :p-length new-l)
         new-s]
        (recur new-s new-v new-l)))))

(defn ->operator-bit-length-packet [pck' cont']
  (let [bit-length (Integer/parseInt (subs cont' 0 15) 2)
        ;; _ (println :bit bit-length)
        [subs cont] (loop [n-bit-left bit-length
                           ps []
                           c' (subs cont' 15)]
                      (if (>= 0 n-bit-left)
                        [ps c']
                        (let [[p c] (->sub-packet c')]
                          (recur (- n-bit-left (:p-length p))
                                 (conj ps p)
                                 c))))]
    [(-> pck'
         (update :p-length + 15 bit-length)
         (assoc :subs subs :sub-bit-length bit-length))
     cont]))

(defn ->operator-sub-count-packet [pck' cont']
  (let [sub-count (Integer/parseInt (subs cont' 0 11) 2)
        ;; _ (println :count sub-count)
        [subs cont] (reduce
                     (fn [[ps c'] _]
                       (let [[p c] (->sub-packet c')]
                         [(conj ps p) c]))
                     [[] (subs cont' 11)]
                     (range sub-count))
        subs-length (reduce + 0 (map :p-length subs))]
    [(-> pck'
         (update :p-length + 11 subs-length)
         (assoc :subs subs :sub-count sub-count))
     cont]))

(defn ->operator-packet [pck' cont']
  (let [length-type-id (first cont')
        pck (-> pck'
                (update :p-length + 1)
                (assoc :length-type-id length-type-id))
        cont (subs cont' 1)]
    ;; (println :op length-type-id)
    (case length-type-id
      \0 (->operator-bit-length-packet pck cont)
      \1 (->operator-sub-count-packet pck cont))))

(defn ->sub-packet [s]
  (let [version (Integer/parseInt (subs s 0 3) 2)
        type-id (Integer/parseInt (subs s 3 6) 2)
        pck {:version version
             :type-id type-id
             :p-length 6}
        cont (subs s 6)]
    ;; (println :sub version type-id (count s))
    (case type-id
      4 (->literal-value-packet pck cont)
      (->operator-packet pck cont))))

(defn ->packet [s]
  (let [[pck cont] (->sub-packet s)
        align (align-bits (:p-length pck))]
    [(update pck :p-length + align)
     (subs cont align)]))

(->packet test-packet1)
;; => [{:version 6, :type-id 4, :p-length 24, :value 2021} ""]

(defn ->version-sum [{:keys [subs version]}]
  (if-not subs
    version
    (+ version (reduce + 0 (map ->version-sum subs)))))

(def test-packet-2
  (->bits "38006F45291200"))
(def test-packet-3
  (->bits "EE00D40C823060"))

(->packet test-packet-2)
;; => [{:version 1, :type-id 6, :p-length 56, :length-type-id \0, :subs [{:version 6, :type-id 4, :p-length 11, :value 10} {:version 2, :type-id 4, :p-length 16, :value 20}], :sub-bit-length 27} ""]
(->packet test-packet-3)
;; => [{:version 7, :type-id 3, :p-length 56, :length-type-id \1, :subs [{:version 2, :type-id 4, :p-length 11, :value 1} {:version 4, :type-id 4, :p-length 11, :value 2} {:version 1, :type-id 4, :p-length 11, :value 3}], :sub-count 3} ""]

(->version-sum
 (first
  (->packet (->bits "8A004A801A8002F478"))))
;; => 16
;; => [{:version 4, :type-id 2, :p-length 72, :length-type-id \1, :subs [{:version 1, :type-id 2, :p-length 51, :length-type-id \1, :subs [{:version 5, :type-id 2, :p-length 33, :length-type-id \0, :subs [{:version 6, :type-id 4, :p-length 11, :value 15}], :sub-bit-length 11}], :sub-count 1}], :sub-count 1} ""]
(->version-sum
 (first
  (->packet (->bits "620080001611562C8802118E34"))))
;; => 12
;; => [{:version 3, :type-id 0, :p-length 104, :length-type-id \1, :subs [{:version 0, :type-id 0, :p-length 44, :length-type-id \0, :subs [{:version 0, :type-id 4, :p-length 11, :value 10} {:version 5, :type-id 4, :p-length 11, :value 11}], :sub-bit-length 22} {:version 1, :type-id 0, :p-length 40, :length-type-id \1, :subs [{:version 0, :type-id 4, :p-length 11, :value 12} {:version 3, :type-id 4, :p-length 11, :value 13}], :sub-count 2}], :sub-count 2} ""]
(->version-sum
 (first
  (->packet (->bits "C0015000016115A2E0802F182340"))))
;; => 23
;; => [{:version 6, :type-id 0, :p-length 112, :length-type-id \0, :subs [{:version 0, :type-id 0, :p-length 44, :length-type-id \0, :subs [{:version 0, :type-id 4, :p-length 11, :value 10} {:version 6, :type-id 4, :p-length 11, :value 11}], :sub-bit-length 22} {:version 4, :type-id 0, :p-length 40, :length-type-id \1, :subs [{:version 7, :type-id 4, :p-length 11, :value 12} {:version 0, :type-id 4, :p-length 11, :value 13}], :sub-count 2}], :sub-bit-length 84} ""]
(->version-sum
 (first
  (->packet (->bits "A0016C880162017C3686B18A3D4780"))))
;; => 31
;; => [{:version 5, :type-id 0, :p-length 120, :length-type-id \0, :subs [{:version 1, :type-id 0, :p-length 91, :length-type-id \1, :subs [{:version 3, :type-id 0, :p-length 73, :length-type-id \1, :subs [{:version 7, :type-id 4, :p-length 11, :value 6} {:version 6, :type-id 4, :p-length 11, :value 6} {:version 5, :type-id 4, :p-length 11, :value 12} {:version 2, :type-id 4, :p-length 11, :value 15} {:version 2, :type-id 4, :p-length 11, :value 15}], :sub-count 5}], :sub-count 1}], :sub-bit-length 91} ""]

(second
 (->packet day16-data))
;; => ""
(->version-sum
 (first
  (->packet day16-data)))
;; => 974

(defn ->packet-value [{:keys [type-id subs value]}]
  (case type-id
    0 (reduce + (map ->packet-value subs))
    1 (reduce * (map ->packet-value subs))
    2 (apply min (map ->packet-value subs))
    3 (apply max (map ->packet-value subs))
    4 value
    5 (if (apply > (map ->packet-value subs)) 1 0)
    6 (if (apply < (map ->packet-value subs)) 1 0)
    7 (if (apply = (map ->packet-value subs)) 1 0)
    0))

(->packet-value (first (->packet (->bits "C200B40A82"))))
;; => 3N
(->packet-value (first (->packet (->bits "04005AC33890"))))
;; => 54N
(->packet-value (first (->packet (->bits "880086C3E88112"))))
;; => 7
(->packet-value (first (->packet (->bits "CE00C43D881120"))))
;; => 9
(->packet-value (first (->packet (->bits "F600BC2D8F"))))
;; => 0
(->packet-value (first (->packet (->bits "D8005AC2A8F0"))))
;; => 1
(->packet-value (first (->packet (->bits "9C005AC2F8F0"))))
;; => 0
(->packet-value (first (->packet (->bits "9C0141080250320F1802104A08"))))
;; => 1

(->packet-value (first (->packet day16-data)))
;; => 180616437720N

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAY 17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test-target
  [[20 30] [-10 -5]])

(defn in-target [[x y] [[min-x max-x] [min-y max-y]]]
  (and (<= min-x x max-x)
       (<= min-y y max-y)))

(in-target [10 5] test-target) ;; => false
(in-target [25 -4] test-target) ;; => false
(in-target [20 -5] test-target) ;; => true

(defn past-target [[x y] [[min-x max-x] [min-y max-y]]]
  (or (< max-x x)
       (< y min-y)))

(past-target [30 -10] test-target) ;; => false
(past-target [30 -11] test-target) ;; => true
(past-target [31 -10] test-target) ;; => true

(defn dv [[vx vy]]
  [(max 0 (dec vx))
   (dec vy)])

(dv [10 10]);; => [9 9]
(dv [1 1]) ;; => [0 0]
(dv [0 0]) ;; => [0 -1]
(dv [0 -1]) ;; => [0 -2]

(defn dp [[x y] [vx vy]]
  [(+ x vx) (+ y vy)])

(dp [0 0] [6 9]);; => [6 9]
(dp [9 0] [0 -9]) ;; => [9 -9]

(defn traj
  ([v]
   (traj [0 0] v))
  ([p v]
   (lazy-seq (cons p (traj (dp p v) (dv v))))))

(take 10 (traj [6 9]))
;; => ([0 0] [6 9] [11 17] [15 24] [18 30] [20 35] [21 39] [21 42] [21 44] [21 45])

(defn hit-target? [v target]
  (in-target
   (first
    (drop-while
     (fn [[x y :as p]]
       (and
        (not (in-target p target))
        (not (past-target p target))))
     (traj v)))
   target))

(hit-target? [6 9] test-target);; => true
(hit-target? [6 8] test-target) ;; => true
(hit-target? [16 8] test-target) ;; => false

(defn v-range [[[min-x max-x] [min-y max-y]]]
  (for [vx (range 1 (inc max-x))
        vy (range min-y (- min-y))]
    [vx vy]))

(count
 (v-range test-target));; => 600

(count
 (filter
  #(hit-target? % test-target)
  (v-range test-target)))
;; => 112

(def day17-target
  [[153 199] [-114 -75]])

(count
 (v-range day17-target))
;; => 45372

(count
 (filter
  #(hit-target? % day17-target)
  (v-range day17-target)))
;; => 3186

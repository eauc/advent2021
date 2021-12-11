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

(require '[clojure.string :as str :refer [split join]])

; Split into words
(defn parse [input]
  (str/split input #"\n"))

; Returns hash-map
(defn countLettersInWord [word]
  (letfn [(f [ct x]
            (assoc ct x
                   (inc (or (get ct x) 0))))]
  (reduce f {} word)))

; Returns pair [c2 c3]
(defn countPairForWord [word]
  (let [ct (vals (countLettersInWord word))]
    [(if (some #{2} ct) 1 0)
     (if (some #{3} ct) 1 0)]))

; Counts checksum
(defn countChecksum [words]
  (apply *
         (apply (partial map +)
                (map countPairForWord words))))

(def part1 countChecksum)


; Distance between two strings (of same length)
(defn distance [s1 s2]
  (reduce +
          (for [[x1 x2] (map vector s1 s2)]
            (if (= x1 x2) 0 1))))

; Returns pair of indexes
(defn findSimilarPair [words]
  (first (remove nil?
                 (let [n (count words)]
                   (for [i (range n) j (range (inc i) n)]
                     (let [d (distance (nth words i) (nth words j))]
                       (if (= d 1) [i j] nil)))))))

; Returns pair of words
(defn findSimilarWords [words]
  (let [[i j] (findSimilarPair words)]
    [(nth words i)
     (nth words j)]))

; Returns index of first difference of two sequences (of same length)
(defn firstDifference [s1 s2]
  (first (remove nil?
                 (for [[i [x1 x2]] (map-indexed vector (map vector s1 s2))]
                   (if (not= x1 x2) i nil)))))

(defn part2 [words]
  (let [[s1 s2] (findSimilarWords words)
        i (firstDifference s1 s2)]
    (str/join (keep-indexed #(if (= %1 i) nil %2) s1))))


(defn solve [input]
  (let [words (parse input)]
    (printf "part1: %d%npart2: %s" (part1 words) (part2 words))))


(let [input (slurp "input")]
  (solve input))

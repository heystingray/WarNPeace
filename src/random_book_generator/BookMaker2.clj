(ns random-book-generator.BookMaker2
  (:require [clojure.string :as string]
            [clojure.java.io :refer [reader]]))

;basically a list of ASCii charcter codes checked against whatever is passed to this function
(defn wanted?
  [c]
  (contains? (apply hash-set (map char (concat (range 48 58) (range 65 91) (range 97 123) [45, 47, 39, 33, 46, 63, 44, 34]))) c))


(def alphanumerics
  (apply hash-set
    (map char (concat (range 48 58) (range 65 91) (range 97 123)))))

;removes all characters not specified in the wanted? condition funciton
(defn simplify
  [coll]
  (apply str(filter wanted? coll)))

;places a space before certain punctuatuion marks so that split-words will divide them into their own strings and map-coll will count them independently
(defn wordify-punct [coll]
  (string/replace (string/replace (string/replace (string/replace (string/replace coll #"\." " .") #"!" " !") #"\?" " ?") #"," " ,") #"\x22" " \" "))

;splits longer strings on whitespace, dashes, and slashes, turning them into a sequence of smaller strings
(defn split-words [coll]
  (string/split coll #"\s+|-+|/"))


(defn book-to-coll [rdr]
  (filter #(re-find #"\S" %) (map simplify (apply concat (map split-words (map wordify-punct (line-seq rdr)))))))

(defn add-word
  [m s]
  (if (contains? m s)
    (update-in m [s :frequency] inc)
    (assoc m s {:frequency 1 :next-words {}})))

(defn add-pair
  [m orig-word pair-word]
  (update-in m [orig-word :next-words] #(add-word % pair-word)))

(defn add-triple
  [m orig-word pair-word trip-word]
  (update-in m [orig-word :next-words] #(add-pair % pair-word trip-word)))

(comment
 (defn build-map
   [coll]
   (reduce #(apply add-triple % %2) (reduce #(apply add-pair % %2) (reduce add-word {} coll) (partition 2 1 coll)) (partition 3 1 coll))))


(defn add-all

  ([m s1 s2 s3]
   (add-triple (add-pair (add-word m s1) s1 s2) s1 s2 s3))

  ([m s1 s2]
   (add-pair (add-word m s1) s1 s2))

  ([m s1]
   (add-word m s1)))


(defn build-map
  [coll]
  (reduce #(apply (partial add-all %) %2) {} (concat (partition 3 1 coll) (list (take-last 2 coll)) (list (list (last coll))))))


(defn update-all
  [m kw f]
  (reduce #(update-in % [%2 kw] f) m (keys m)))

(defn accumulate
  [m]
  (loop [final-map m
         [k & remaining] (keys m)
         accum (get-in final-map [k :frequency])
         _ '(println accum)]
    (if (empty? remaining)
      (assoc-in final-map [k :frequency] accum)
      (recur (assoc-in final-map [k :frequency] accum)
             remaining
             (+ accum (get-in final-map [(first remaining) :frequency]))
             '(println (+ accum (get-in final-map [(first remaining) :frequency])))))))

(defn accumulate-pairs
  [m]
  (update-all m :next-words accumulate))

(defn accumulate-triples
  [m]
  (update-all m :next-words accumulate-pairs))

(defn accumulate-all
  [m]
  (accumulate-triples (accumulate-pairs (accumulate m))))

(defn get-word
  [m]
  (rand-nth (reduce #(into % (repeat (:frequency (get m %2)) %2)) [] (keys m))))

(defn get-pair
  [m orig-word]
  (get-word (get-in m [orig-word :next-words])))

(defn get-triple
  [m orig-word pair-word]
  (get-pair (get-in m [orig-word :next-words]) pair-word))

;(with-open [rdr (reader "WarandPeace.txt")]
  ;(build-map (book-to-coll rdr)))

(defn generate-words
  [m n]
  (loop [s1 (rand-nth [(get-pair m "."), (get-pair m "!"), (get-pair m "?")])
         s2 (get-pair m s1)
         final-seq [s1, s2]
         count n
         _ '(println count)]
        (if (= count 0)
         final-seq
         (let [new-word (get-triple m s1 s2)]
             (recur s2 new-word (conj final-seq new-word) (dec count) '(println count))))))

(defn recombine
  [s x]
  (if (#{"." "," "'" "!" "?"} x)
    (str s x)
    (str s " " x)))


(defn coll-from-file
  [filename]
  (with-open [rdr (reader filename)]
    (doall (book-to-coll rdr))))

(defn make-book
  [m n]
  (let [book (generate-words m n)]
   (reduce recombine (first book) (rest book))))

(defn make-book-from-file
  [filename n]
  (with-open [rdr (reader filename)]
    (make-book (build-map (book-to-coll rdr)) n)))

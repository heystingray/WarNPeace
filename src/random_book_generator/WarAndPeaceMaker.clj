(ns random-book-generator.WarAndPeaceMaker
  (:require [clojure.string :as string]
            [clojure.java.io :refer [reader]]))

(defn wanted?
  [c]
  (contains? (apply hash-set (map char (concat (range 48 58) (range 65 91) (range 97 123) [45, 47, 39, 33, 46, 63]))) c))


(defn simplify
  [coll]
  (apply str(filter wanted? coll)))


(defn count-letter [c coll]
 ;"filter" returns a sequence of only the members of string "coll" which match
 ;"c" then "count" returns the length of that sequence
  (count (filter #{c} coll)))

(defn map-coll [coll]
 ;for is a sequence comprehension that takes each letter "c" in string "coll",
 ;uses "hash-map" to create a map with "c" as the key and the number of times it appears in coll
 ;the value. Then "apply merge" combines the sequence of maps returned by for into one map, removing repititions.
  (apply merge (for [c coll]
                (hash-map c (count-letter c coll)))))



(defn split-words [coll]
  (string/split coll #"\s+|-+|/"))

(defn map-coll-n [n coll]
 ;string/split returns a sequence of the words in coll by splitting on whitespace
 ;then map-coll is applied to the sequence of words
  (map-coll (take n coll)))




(defn map-pattern [n coll]
  (map-coll (partition n 1 (split-words coll))))


(defn wordify-end-punct [coll]
  (string/replace (string/replace (string/replace coll #"\." " .") #"!" " !") #"\?" " ?"))

(defn un-wordify-end-punct [coll]
  (string/replace (string/replace (string/replace coll #" \." ".") #" !" "!") #" \?" "?"))

(comment
 (def WPcoll
   (with-open [rdr (reader "C:\\ProClo\\WarandPeace.txt")]
       (filter #(re-find #"\S" %) (map simplify (apply concat (map split-words (map wordify-end-punct (doall (line-seq rdr))))))))))


(def WPcoll '("This" "is" "a" "fake" "set" "of" "words" "." "We" "are" "using" "it" "for" "testing" "!" "This" "is" "a" "test" "."))


(defn better-map-coll [coll]
  (loop [remaining-words coll final-map {}]
    (if (empty? remaining-words)
      final-map
      (let [[word & remaining] remaining-words]
        (recur remaining
          (if (contains? final-map word)
            ;(add 1 to key value)
            (update-in final-map [word] inc)
            (assoc final-map word 1)))))))

(defn better-map-pattern [n coll]
  (better-map-coll (partition n 1 coll)))

(def WPmap1 (better-map-pattern 1 WPcoll))

(def WPmap2 (better-map-pattern 2 WPcoll))

(def WPmap3 (better-map-pattern 3 WPcoll))

(defn total-vals [m]
  (apply + (vals m)))



(defn pick-key-by-value [m]
  (let [items (keys m)
        total (reduce + (vals m))
        pick (rand total)]
    (loop [[item & remaining] items
            accumulation (m item)]
      (if (> accumulation pick)
        item
        (recur remaining (+ accumulation (m (first remaining))))))))



(defn purge-zeroes [m]
  (apply dissoc m (remove #(< 0 (m %)) (keys m))))

(defn single-to-pair [m word]
  (apply dissoc m (remove #(= (first %) word) (keys m))))

(defn pair-to-triple [m words]
  (apply dissoc m (remove #(and (= (first %) (first words)) (= (second %) (second words))) (keys m))))

(defn match-last [m word]
  (filter #(= (last %) word) (keys m)))

;(println "start")

(defn generate-words [coll]
  (loop [map1 (better-map-pattern 1 coll)
         map2 (better-map-pattern 2 coll)
         map3 (better-map-pattern 3 coll)
         final-book []
         choice (pick-key-by-value map1)]
        (if (empty? map1) ;if map1 is empty, return the final book
          final-book
          ;otherwise, recur
          ;(do (println (last choice))
          (let [new-map1 (if (contains? map1 (list (last choice)))
                           (purge-zeroes (update map1 (list (last choice)) dec))
                           map1)


                new-map2 (purge-zeroes (reduce #(update %1 %2 dec) map2 (match-last map2 (last choice))))



                new-map3 (purge-zeroes (reduce #(update %1 %2 dec) map3 (match-last map3 (last choice))))]

           (recur ;map1
                  new-map1

                  ;map2
                  new-map2

                  ;map3
                  new-map3

                  ;final-book
                  (if (or (empty? final-book) (= (last choice) ".") (= (last choice) "!") (= (last choice) "?"))
                   (conj final-book (last choice))
                   (conj final-book " " (last choice)))

                  ;choice

                  ;if previous choice was one word choose a pair
                  (if (and (= (count choice) 1) (not (empty? (single-to-pair new-map2 choice))))
                     (pick-key-by-value (single-to-pair new-map2 choice))

                     ;if previous choice was two words choose a triple
                     (if (and (= (count choice) 2) (not (empty? (pair-to-triple new-map3 choice))))
                       (pick-key-by-value (pair-to-triple new-map3 choice))

                       ;if previous choice was a triple choose a triple based on the last two words
                       (if (and (= (count choice) 3) (not (empty? (pair-to-triple new-map3 (rest choice)))))
                         (pick-key-by-value (pair-to-triple new-map3 (rest choice)))

                         ;if there are no triple options pick a pair based on the last word
                         (if (not (empty? (single-to-pair new-map2 (last choice))))
                           (pick-key-by-value (single-to-pair new-map2 (last choice)))

                           ;if you can't pick a pair or triple, pick a single word
                           (if (not (empty? new-map1))
                             (pick-key-by-value new-map1)

                             ;if you're out of single words, choice is set to nil
                             nil))))))))));)

(defn book-to-coll [rdr]
  (filter #(re-find #"\S" %) (map simplify (apply concat (map split-words (map wordify-end-punct (line-seq rdr)))))))

(defn make-book [filename n]
  (apply str
    (with-open [rdr (reader filename)]
      (generate-words (take n (book-to-coll rdr))))))

(ns random-book-generator.BookMaker
  (:require [clojure.string :as string]
            [clojure.java.io :refer [reader]]))

;basically a list of ASCii charcter codes checked against whatever is passed to this function
(defn wanted?
  [c]
  (contains? (apply hash-set (map char (concat (range 48 58) (range 65 91) (range 97 123) [45, 47, 39, 33, 46, 63, 44, 34]))) c))


;removes all characters not specified in the wanted? condition funciton
(defn simplify
  [coll]
  (apply str(filter wanted? coll)))

;used by map-coll, goes through entire collection and counts instance of c
(defn count-letter [c coll]
 ;"filter" returns a sequence of only the members of string "coll" which match
 ;"c" then "count" returns the length of that sequence
  (count (filter #{c} coll)))

;slower, n^2 version
(defn map-coll [coll]
 ;for is a sequence comprehension that takes each letter "c" in string "coll",
 ;uses "hash-map" to create a map with "c" as the key and the number of times it appears in coll
 ;the value. Then "apply merge" combines the sequence of maps returned by for into one map, removing repititions.
  (apply merge (for [c coll]
                (hash-map c (count-letter c coll)))))


;splits longer strings on whitespace, dashes, and slashes, turning them into a sequence of smaller strings
(defn split-words [coll]
  (string/split coll #"\s+|-+|/"))

;version of map-coll that only considers the first n items in the given collection
(defn map-coll-n [n coll]
  (map-coll (take n coll)))


;extension of map-coll which uses partition to apply the mapping to patterns of n members with a step of one, 2 for pairs, 3 for triples, etc
(defn map-pattern [n coll]
  (map-coll (partition n 1 (split-words coll))))

;places a space before certain punctuatuion marks so that split-words will divide them into their own strings and map-coll will count them independently
(defn wordify-punct [coll]
  (string/replace (string/replace (string/replace (string/replace (string/replace coll #"\." " .") #"!" " !") #"\?" " ?") #"," " ,") #"\x22" " \" "))

;reverses the effect of the previous function, puttting punctuation marks in their rightful place
;didn't actually end up being utilized in this program
(defn un-wordify-end-punct [coll]
  (string/replace (string/replace (string/replace coll #" \." ".") #" !" "!") #" \?" "?"))

;following collection used for practice, if uncommented it increases the loadtime of this file by about 70 seconds
(comment
 (def WPcoll
   (with-open [rdr (reader "C:\\ProClo\\WarandPeace.txt")]
       (filter #(re-find #"\S" %) (map simplify (apply concat (map split-words (map wordify-punct (doall (line-seq rdr))))))))))


;(def WPcoll '("This" "is" "a" "fake" "set" "of" "words" "." "We" "are" "using" "it" "for" "testing" "!" "This" "is" "a" "test" "."))

;improved linear version of map-coll, much faster on large collections
(defn better-map-coll [coll]
  (loop [remaining-words coll final-map {}] ;initial binding takes full collection and empty map
    (if (empty? remaining-words)
      ;if the collection is empty return the map
      final-map
      (let [[word & remaining] remaining-words] ;split collection into head and tail
        (recur remaining ;feed tail into loop

              (if (contains? final-map word)

                (update-in final-map [word] inc) ;if head is already in map, increment its value
                (assoc final-map word 1))))))) ;if head is not in map, add it in with val of 1

;implements better version of map-coll onto patterns of n, similar to map-pattern
(defn better-map-pattern [n coll]
  (better-map-coll (partition n 1 coll)))

;(def WPmap1 (better-map-pattern 1 WPcoll))

;(def WPmap2 (better-map-pattern 2 WPcoll))

;(def WPmap3 (better-map-pattern 3 WPcoll))



;picks a key from the map based on size of numeric value, more likely to pick keys with high values
(defn pick-key-by-value [m]
  (let [items (keys m) ;seq of all keys in map
        total (reduce + (vals m)) ;total of all vals in map
        pick (rand total)] ;random number from 0 to total
    (loop [[item & remaining] items ;split into head and tail
            accumulation (m item)]  ;set initial value of the accumulation to the value of the first item
      (if (> accumulation pick)
        item                    ;return chosen item once accumulation is over
        (recur remaining (+ accumulation (m (first remaining))))))))  ;feed remaining back to loop and add to accumulation


;this function removes all items from a map which has value 0
(defn purge-zeroes [m]
  (apply dissoc m (remove #(< 0 (m %)) (keys m))))

;returns the section of map m  where the keys begin with word
(defn single-to-pair [m word]
  (apply dissoc m (remove #(= (first %) word) (keys m))))

;returns the section of map m where the keys begin with the first and second items of words
(defn pair-to-triple [m words]
  (apply dissoc m (remove #(and (= (first %) (first words)) (= (second %) (second words))) (keys m))))

;returns the keys of map m whose last word matches word
(defn match-last [m word]
  (filter #(= (last %) word) (keys m)))

;UP TO THIS POINT WAS BOTH MALLORY AND ME, AFTER THIS IS THE GENERATOR I WROTE THAT UTILIZES THE FUNCTIONS WE BUILT, MALLORY WANTED TO CREATE HER IMPLEMENTATION INDEPENDENTLY
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;(println "start")

;takes a collection of words and randomly generates them into a new order based on frequency and patterns of pairs and triples
(defn generate-words [coll]
  ;to start the loop we pick our initial bindings
  (loop [map1 (better-map-pattern 1 coll)
         map2 (better-map-pattern 2 coll) ;each of these is bound to a map of single, pairs, and triples from the collection of words
         map3 (better-map-pattern 3 coll)
         final-book []                    ;final book starts as an empty vector to be continually filled
         choice (pick-key-by-value map1)]

        (if (empty? map1) ;if map1 is empty, return the final book
          final-book
          ;otherwise, recur
          ;(do (println (last choice))

          ;updating maps is done in a let binding before the loop recursion so that the next choice is made with most recent information
          ;each of these bindings basically updates each map to reflect the fact that after a choice is made, the available options in each map reduces by one
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
                  (if (or (empty? final-book) (= (last choice) ".") (= (last choice) "!") (= (last choice) "?") (= (last choice) "\"") (= (last choice) ","))
                   (conj final-book (last choice))
                   (conj final-book " " (last choice)))

                  ;choice

                  ;if previous choice was one word and pairs are available choose a pair
                  (if (and (= (count choice) 1) (not (empty? (single-to-pair new-map2 choice))))
                     (pick-key-by-value (single-to-pair new-map2 choice))

                     ;if previous choice was two words and triples are available choose a triple
                     (if (and (= (count choice) 2) (not (empty? (pair-to-triple new-map3 choice))))
                       (pick-key-by-value (pair-to-triple new-map3 choice))

                       ;if previous choice was a triple and triples are available choose a triple based on the last two words
                       (if (and (= (count choice) 3) (not (empty? (pair-to-triple new-map3 (rest choice)))))
                         (pick-key-by-value (pair-to-triple new-map3 (rest choice)))

                         ;if there are no triple options and pairs are available pick a pair based on the last word
                         (if (not (empty? (single-to-pair new-map2 (last choice))))
                           (pick-key-by-value (single-to-pair new-map2 (last choice)))

                           ;if you can't pick a pair or triple, pick a single word if you can
                           (if (not (empty? new-map1))
                             (pick-key-by-value new-map1)

                             ;if you're out of single words, choice is set to nil, the final-book will return on next loop
                             nil))))))))));)

;takes a buffered file reader and creates a collection of strings from the lines in the file
(defn book-to-coll [rdr]
  (filter #(re-find #"\S" %) (map simplify (apply concat (map split-words (map wordify-punct (line-seq rdr)))))))

;experimental alternate implementation, not used
(defn book-to-coll2 [rdr]
  (apply concat (map split-words (map wordify-punct (filter #(re-find #"\S" %) (line-seq rdr))))))

;combines book-to-coll and generate-words
;takes a filename of a book, uses it to create a collection of words, then feeds that collection to generate-words
(defn make-book

  ([filename]
   (apply str
     (with-open [rdr (reader filename)]
       (generate-words (book-to-coll rdr)))))

  ;if you give it a filename and a number, it only takes the first n words of the original file
  ([filename n]
   (apply str
     (with-open [rdr (reader filename)]
       (generate-words (take n (book-to-coll rdr)))))))

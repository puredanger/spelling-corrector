(ns spelling.core
  (:require [clojure.string :as str]))

(def ^:constant alphabet "abcdefghijklmnopqrstuvwxyz")

(defn words [text]
  (re-seq #"[a-z]+" (str/lower-case text)))

(defn train [words]
  (frequencies words))

(def NWORDS (train (words (slurp "big.txt"))))

(defn edits1 [word]
  (let [splits (for [i (range (inc (count word)))] [(subs word 0 i) (subs word i)])
        deletes (for [[a b] splits :when (not (str/blank? b))] (str a (subs b 1)))
        transposes (for [[a b] splits :when (> (count b) 1)] (str a (second b) (first b) (subs b 2)))
        replaces (for [[a b] splits :when (> (count b) 0)
                       c alphabet]
                   (str a c (subs b 1)))
        inserts (for [[a b] splits
                      c alphabet]
                  (str a c b))]
    (set (concat deletes transposes replaces inserts))))

(defn edits2 [word]
  (set
   (for [e1 (edits1 word)
         e2 (edits1 e1)]
     e2)))

(defn known-edits2 [word]
  (set
   (for [e1 (edits1 word)
         e2 (edits1 e1)
         :when (NWORDS e2)]
     e2)))

(defn known [words]
  (set (for [w words :when (NWORDS w)] w)))

(defn correct [word]
  (let [candidates [(known [word])
                    (known (edits1 word))
                    (known-edits2 word)
                    [word]]
        selected-candidates (first (remove empty? candidates))]
    (last (sort-by NWORDS selected-candidates))))

(ns adventofcode2016.day-7
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(def letters "abcdefghijklmnopqrstuvwxyz")

(defn load-input
  []
  (string/split (slurp "resources/input.7") #"\n"))

(defn make-abbas
  [letters]
  (set
    (for [a letters
          b letters
          :when (not= a b)]
      (string/join [a b b a]))))

(defn make-abas
  [letters]
  (set
    (for [a letters
          b letters
          :when (not= a b)]
      (string/join [a b a]))))

(def abbas (make-abbas letters))
(def abas (make-abas letters))

(defn contains-abba?
  [str]
  (some #(string/includes? str %) abbas))

(defn parse-hypernet-seqs
  [ip]
  (map last (re-seq #"\[(\w+)\]" ip)))

(defn parse-supernet-seqs
  [ip]
  (string/split (string/replace ip #"\[\w*\]" " ") #" "))

(defn supports-tls?
  [ip]
  (let [hypernet-seqs (parse-hypernet-seqs ip)
        supernet-seqs (parse-supernet-seqs ip)]
    (and
      (some contains-abba? supernet-seqs)
      (not (some contains-abba? hypernet-seqs)))))

(defn contained-abas
  [str]
  (filter #(string/includes? str %) abas))

(defn aba->bab
  [aba]
  (string/join [(second aba) (first aba) (second aba)]))

(defn supports-ssl?
  [ip]
  (let [hypernet-seqs     (parse-hypernet-seqs ip)
        supernet-seqs     (parse-supernet-seqs ip)
        abas-in-hypernets (mapcat contained-abas hypernet-seqs)
        babs-to-look-for  (map aba->bab abas-in-hypernets)]
    (some (fn [supernet-seq]
            (some #(string/includes? supernet-seq %) babs-to-look-for))
          supernet-seqs)))

(defn run
  [filter-fn]
  (->>
    (map filter-fn (load-input))
    (filter identity)
    (count)))

(defn run-pt1
  []
  (run supports-tls?))

(defn run-pt1
  []
  (run supports-ssl?))

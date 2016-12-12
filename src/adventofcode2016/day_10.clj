(ns adventofcode2016.day-10
  (:require [clojure.string :as string]))

(defn- load-input
  []
  (sort (fn [a b]
          ; "bot" commands apparently must be completed before "value" ones
          (compare (first a) (first b)))
        (mapv #(string/split % #"\s")
              (string/split (slurp "resources/input.10") #"\n"))))

(declare shuffle-chips)

(def run-instruction nil)
(defmulti run-instruction (fn [acc instr]
                            (keyword (first instr))))

(defn- read-int-from-vec
  [vec n]
  (Integer/parseInt (vec n)))

(defmethod run-instruction :bot
  [acc instr]
  (let [this-bot-num       (read-int-from-vec instr 1)
        bot-name           (str "bot" this-bot-num)
        low-output-or-bot  (instr 5)
        low-num            (read-int-from-vec instr 6)
        high-output-or-bot (instr 10)
        high-num           (read-int-from-vec instr 11)]
    (update-in acc [bot-name] merge {:low  (str low-output-or-bot low-num)
                                     :high (str high-output-or-bot high-num)})))

(defn- chips-for-bot
  [acc bot-name]
  (or (:chips (acc bot-name)) []))

(defn- add-chip-to-bot
  [acc output-name val]
  (let [existing-chips (chips-for-bot acc output-name)]
    (println "add" val "to" output-name ":" existing-chips (acc output-name) {output-name (merge (acc output-name) {:chips (conj existing-chips val)})})
    (merge acc {output-name (merge (acc output-name) {:chips (conj existing-chips val)})})))

(defn- shuffle-if-full
  [acc output-name]
  (if (and (string/starts-with? output-name "bot")
           (= (count (chips-for-bot acc output-name)) 2))
    (shuffle-chips acc output-name)
    acc))

(defn- shuffle-chips
  [acc bot-name]
  (let [bot-data (acc bot-name)
        vals     (sort (:chips bot-data))
        new-acc  (assoc-in acc [bot-name :chips] [])]
    (println "shuffle bot" bot-data)
    (-> new-acc
        (add-chip-to-bot (:low bot-data) (first vals))
        (shuffle-if-full (:low bot-data))
        (add-chip-to-bot (:high bot-data) (second vals))
        (shuffle-if-full (:high bot-data)))))

(defmethod run-instruction :value
  [acc instr]
  (let [val        (read-int-from-vec instr 1)
        to-bot-num (read-int-from-vec instr 5)
        bot-name   (str "bot" to-bot-num)
        new-acc    (add-chip-to-bot acc bot-name val)]
    (shuffle-if-full new-acc bot-name)))

(defn run
  []
  (reduce run-instruction {} (load-input)))
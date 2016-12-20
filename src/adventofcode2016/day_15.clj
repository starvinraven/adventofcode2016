(ns adventofcode2016.day-15)

(def initial-state-pt1
  {:time  0
   :discs [{:positions 5 :at 2}
           {:positions 13 :at 7}
           {:positions 17 :at 10}
           {:positions 3 :at 2}
           {:positions 19 :at 9}
           {:positions 7 :at 0}]})

(def initial-state-pt2
  (update-in initial-state-pt1 [:discs] conj {:positions 11 :at 0}))

(defn desired-positions
  [discs]
  (map-indexed
    (fn [idx {:keys [positions]}]
      (mod (+ positions (- positions (inc idx))) positions))
    discs))

(defn advance
  [{:keys [time discs]}]
  {:time  (inc time)
   :discs (map
            (fn [{:keys [positions at]}]
              {:positions positions
               :at        (mod (inc at) positions)})
            discs)})

(defn- run
  [initial-state]
  (let [opportune-state (desired-positions (:discs initial-state))]
    (loop [state initial-state]
      (if (= (map :at (:discs state)) opportune-state)
        (println "got it at" (:time state))
        (recur (advance state))))))

(def run-pt1 (partial run initial-state-pt1))
(def run-pt2 (partial run initial-state-pt2))
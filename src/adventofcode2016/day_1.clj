(ns adventofcode2016.day-1)

; generate input = s/(\w)(\d+)/[:$1 $2]/g
(def input [[:R 1], [:L 3], [:R 5], [:R 5], [:R 5], [:L 4], [:R 5], [:R 1], [:R 2], [:L 1], [:L 1], [:R 5], [:R 1], [:L 3], [:L 5], [:L 2], [:R 4], [:L 1], [:R 4], [:R 5], [:L 3], [:R 5], [:L 1], [:R 3], [:L 5], [:R 1], [:L 2], [:R 1], [:L 5], [:L 1], [:R 1], [:R 4], [:R 1], [:L 1], [:L 3], [:R 3], [:R 5], [:L 3], [:R 4], [:L 4], [:R 5], [:L 5], [:L 1], [:L 2], [:R 4], [:R 3], [:R 3], [:L 185], [:R 3], [:R 4], [:L 5], [:L 4], [:R 48], [:R 1], [:R 2], [:L 1], [:R 1], [:L 4], [:L 4], [:R 77], [:R 5], [:L 2], [:R 192], [:R 2], [:R 5], [:L 4], [:L 5], [:L 3], [:R 2], [:L 4], [:R 1], [:L 5], [:R 5], [:R 4], [:R 1], [:R 2], [:L 3], [:R 4], [:R 4], [:L 2], [:L 4], [:L 3], [:R 5], [:R 4], [:L 2], [:L 1], [:L 3], [:R 1], [:R 5], [:R 5], [:R 2], [:L 5], [:L 2], [:L 3], [:L 4], [:R 2], [:R 1], [:L 4], [:L 1], [:R 1], [:R 5], [:R 3], [:R 3], [:R 4], [:L 1], [:L 4], [:R 1], [:L 2], [:R 3], [:L 3], [:L 2], [:L 1], [:L 2], [:L 2], [:L 1], [:L 2], [:R 3], [:R 1], [:L 4], [:R 1], [:L 1], [:L 4], [:R 1], [:L 2], [:L 5], [:R 3], [:L 5], [:L 2], [:L 2], [:L 3], [:R 1], [:L 4], [:R 1], [:R 1], [:R 2], [:L 1], [:L 4], [:L 4], [:R 2], [:R 2], [:R 2], [:R 2], [:R 5], [:R 1], [:L 1], [:L 4], [:L 5], [:R 2], [:R 4], [:L 3], [:L 5], [:R 2], [:R 3], [:L 4], [:L 1], [:R 2], [:R 3], [:R 5], [:L 2], [:L 3], [:R 3], [:R 1], [:R 3]])
(def initial-state {:dir 0 :x 0 :y 0 :visiteds #{}})

(defn new-direction
  [current-dir turn-dir]
  (let [op (case turn-dir
             :L -
             :R +)]
    (mod (Math/abs (op (+ 360 current-dir) 90)) 360)))

(defn points-between
  [x1 y1 x2 y2]
  (if (= x1 x2)
    (map (fn [y] [x1 y]) (range (+ 1 (min y1 y2)) (max y1 y2)))
    (map (fn [x] [x y1]) (range (+ 1 (min x1 x2)) (max x1 x2)))))

(defn displacement
  [direction dist]
  (merge {:x 0 :y 0}
         (cond
           (= direction 90) {:x dist}
           (= direction 270) {:x (- dist)}
           (= direction 0) {:y dist}
           (= direction 180) {:y (- dist)})))

(defn run []
      (let [result (reduce (fn [{:keys [dir x y visiteds]} [turn dist]]
                               (let [new-dir (new-direction dir turn)
                                     delta (displacement new-dir dist)
                                     new-x (+ x (:x delta))
                                     new-y (+ y (:y delta))
                                     new-visiteds (points-between x y new-x new-y)]

                                    (println turn dist "->" new-dir (str "(" new-x ", " new-y ")") new-visiteds)

                                    (doseq [new-visited new-visiteds]
                                           (when (contains? visiteds new-visited)
                                                 (println "revisit!" new-visited)))

                                    {:dir      new-dir
                                     :x        new-x
                                     :y        new-y
                                     :visiteds (into visiteds new-visiteds)}))
                           initial-state
                           input)
            dist (+ (Math/abs (:x result)) (Math/abs (:y result)))]
           (println "result:" result)
           dist))


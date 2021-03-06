(ns adventofcode2016.day-2)

(def input-raw
  "RDLRUUULRRDLRLLRLDDUDLULULDDULUDRRUURLRLLUULDURRULLRULDRRDLLULLRLLDRLDDRRRRLLRLURRRDRDULRDUULDDDULURUDDRRRUULUDRLLUUURLUDRUUUDRDUULLRLLUDDRURRDDDRDLUUURLRLLUDRURDUDUULDDLLRDURULLLURLDURLUUULDULDDULULLLRRUDLRUURDRDLLURLUDULDUUUURRLDLUDRULUDLDLLDRLDDDRRLLDUDLLRRDDDRLUDURLLLDRUDDLDDRRLUDRRDUDLRRLULDULURULDULUULDRLLDRUUDDRLLUDRULLRRRLRDLRLUDLRULDRDLRDRLRULUDUURRUUULLDDDDUDDLDDDDRRULRDLRDDULLDLDLLDLLDLLDRRDDDRDDLRRDDDRLLLLURRDLRRLDRURDDURDULDDRUURUDUDDDRDRDDRLRRLRULLDRLDLURLRLRUDURRRDLLLUDRLRDLLDDDLLUDRLDRRUUDUUDULDULLRDLUDUURLDDRUDR
URULDDLDDUDLLURLUUUUUULUDRRRDDUDURDRUURLLDRURLUULUDRDRLLDRLDULRULUURUURRLRRDRUUUDLLLLRUDDLRDLLDUDLLRRURURRRUDLRLRLLRULRLRLRDLRLLRRUDDRLRUDULDURDLDLLLRDRURURRULLLDLLRRDRLLDUUDLRUUDDURLLLDUUDLRDDURRDRRULLDRLRDULRRLLRLLLLUDDDRDRULRRULLRRUUDULRRRUDLLUUURDUDLLLURRDDUDLDLRLURDDRRRULRRUDRDRDULURULRUDULRRRLRUDLDDDDRUULURDRRDUDLULLRUDDRRRLUDLRURUURDLDURRDUUULUURRDULLURLRUUUUULULLDRURULDURDDRRUDLRLRRLLLLDDUURRULLURURRLLDRRDDUUDLLUURRDRLLLLRLUDUUUDLRLRRLDURDRURLRLRULURLDULLLRRUUUDLLRRDUUULULDLLDLRRRDUDDLRULLULLULLULRU
DURUUDULRRLULLLDDUDDLRRDURURRRDDRRURDRURDRLULDUDUDUULULDDUURDDULRDUDUDRRURDRDDRLDRDRLDULDDULRULLDULURLUUDUDULRDDRRLURLLRRDLLDLDURULUDUDULDRLLRRRUDRRDDDRRDRUUURLDLURDLRLLDUULLRULLDDDDRULRRLRDLDLRLUURUUULRDUURURLRUDRDDDRRLLRLLDLRULUULULRUDLUDULDLRDDDDDRURDRLRDULRRULRDURDDRRUDRUDLUDLDLRUDLDDRUUULULUULUUUDUULDRRLDUDRRDDLRUULURLRLULRURDDLLULLURLUDLULRLRRDDDDDRLURURURDRURRLLLLURLDDURLLURDULURUUDLURUURDLUUULLLLLRRDUDLLDLUUDURRRURRUUUDRULDDLURUDDRRRDRDULURURLLDULLRDDDRRLLRRRDRLUDURRDLLLLDDDDLUUURDDDDDDLURRURLLLUURRUDLRLRRRURULDRRLULD
LLUUURRDUUDRRLDLRUDUDRLRDLLRDLLDRUULLURLRRLLUDRLDDDLLLRRRUDULDLLLDRLURDRLRRLURUDULLRULLLURRRRRDDDLULURUDLDUDULRRLUDDURRLULRRRDDUULRURRUULUURDRLLLDDRDDLRRULRDRDRLRURULDULRRDRLDRLLDRDURUUULDLLLRDRRRLRDLLUDRDRLURUURDLRDURRLUDRUDLURDRURLRDLULDURDDURUUDRLULLRLRLDDUDLLUUUURLRLRDRLRRRURLRULDULLLLDLRRRULLUUDLDURUUUDLULULRUDDLLDLDLRLDDUDURDRLLRRLRRDDUDRRRURDLRLUUURDULDLURULUDULRRLDUDLDDDUUDRDUULLDDRLRLLRLLLLURDDRURLDDULLULURLRDUDRDDURLLLUDLLLLLUDRDRDLURRDLUDDLDLLDDLUDRRDDLULRUURDRULDDDLLRLDRULURLRURRDDDRLUUDUDRLRRUDDLRDLDULULDDUDURRRURULRDDDUUDULLULDDRDUDRRDRDRDLRRDURURRRRURULLLRRLR
URLULLLDRDDULRRLRLUULDRUUULDRRLLDDDLDUULLDRLULRRDRRDDDRRDLRRLLDDRDULLRRLLUDUDDLDRDRLRDLRDRDDUUDRLLRLULLULRDRDDLDDDRLURRLRRDLUDLDDDLRDLDLLULDDRRDRRRULRUUDUULDLRRURRLLDRDRRDDDURUDRURLUDDDDDDLLRLURULURUURDDUDRLDRDRLUUUULURRRRDRDULRDDDDRDLLULRURLLRDULLUUDULULLLLRDRLLRRRLLRUDUUUULDDRULUDDDRRRULUDURRLLDURRDULUDRUDDRUURURURLRDULURDDDLURRDLDDLRUDUUDULLURURDLDURRDRDDDLRRDLLULUDDDRDLDRDRRDRURRDUDRUURLRDDUUDLURRLDRRDLUDRDLURUDLLRRDUURDUDLUDRRL")

(def input (map
             (fn [line] (map (comp keyword str) line))
             (map
               seq
               (clojure.string/split input-raw #"[^RDLU]"))))

(def transitions-1 {:1 {:R :2 :D :4}
                    :2 {:R :3 :D :5 :L :1}
                    :3 {:D :6 :L :2}
                    :4 {:U :1 :R :5 :D :7}
                    :5 {:U :2 :R :6 :D :8 :L :4}
                    :6 {:U :3 :D :9 :L :5}
                    :7 {:U :4 :R :8}
                    :8 {:U :5 :R :9 :L :7}
                    :9 {:U :6 :L :8}})

(def transitions-2 {:1 {:D :2}
                    :2 {:R :3 :D :6}
                    :3 {:U :1 :R :4 :D :7 :L :2}
                    :4 {:D :8 :L :3}
                    :5 {:R :6}
                    :6 {:U :2 :R :7 :D :A :L :5}
                    :7 {:U :3 :R :8 :D :B :L :6}
                    :8 {:U :4 :R :9 :D :C :L :7}
                    :9 {:L :8}
                    :A {:U :6 :R :B}
                    :B {:U :7 :R :C :D :D :L :A}
                    :C {:U :8 :L :B}
                    :D {:U :B}})

(def initial-state {:code "" :last-button :5})

(defn walk-keyboard
  [start-button instructions transitions]
  (reduce (fn [current-button instruction]
            (or
              (instruction (current-button transitions))
              current-button)) start-button instructions))

(defn run
  [transitions]
  (reduce (fn [{:keys [code last-button]} instr]
            (let [this-code (walk-keyboard last-button instr transitions)]
              {:code        (str code (name this-code))
               :last-button this-code})) initial-state input))

(def run-pt1 (partial run transitions-1))
(def run-pt2 (partial run transitions-2))
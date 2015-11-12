(ns clojure0305ja.core
  (:use clojure.pprint))           ;koriscenje pprint komande
(import
    '(java.awt Color))    ;za rgb boje 

(defn empty-board         ;kreiranje prazne tabele
  "Creates a rectangular empty board of the specified width and height."
  [w h]                                     ;ako bi smo ovo stavili u red sa imenom funkcije onda bi 
                                            ;stampa svake nil vrenodsti izlazila u novom redu
  (vec (repeat w (vec (repeat h nil)))))     ;vec - formira vector 
;(empty-board 2 3)
;[[nil nil nil] [nil nil nil]]

#_((empty-board 3 6)
[[nil nil nil nil nil nil]
 [nil nil nil nil nil nil]
 [nil nil nil nil nil nil]])

(defn populate      ;menjanje odredjenih polja sa vrednosti nil na :on vrednostima
  "Turns :on each of the cells specified as [y, x] coordinates."
  [board living-cells]
  ;(println "populate")
  (reduce                      ;
    (fn [board coordinates]            ;board-je doneta vrednost, coordinates je vrednost koja se racuna u hodu i upisuje se 
                                             ;u ovom slucaju u board
      (assoc-in board coordinates :on));assoc-in neku ugnjezdenu vrednosr ispravlja. u ovom slucaju to je doneta vrednost pri 
                                          ;pozivu funkcije #{[2 0] [2 1] [2 2] [1 2] [0 1]}
          board                        ;u sta se transformise rezultat funkcije reduce tj u sta se upisuje reultat izvrsenja assoc-in
          living-cells))               ;koje se ugnjezdene vrednosti uzimaju u obzir za promenu.

#_(defn reduce2 [& args]
  (reduce
    (fn [m v]                 ;m-uzima vrednosti iz vectora [1 2 3 4 5] respektivno
      (assoc m v (* v v)))    ;assoc-pravi parove m v gde se v racuna kao v na kvadrat
    {}                        ;i sve se to smesta u map
    [1 2 3 5]))               ;ulazne vrednosti zs funkciju fn (m)
;(reduce2)
;{5 25, 3 9, 2 4, 1 1}

(def glider (populate (empty-board 7 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))  ;dodavanje zivih polja u praznu tablu 6x6

;(pprint glider)                        ;za PPRINT komandu mora se koristiti :use clojure.pprint ispod omena fajla sa programom
#_(
    (pprint (for [x (range 10)] (range x)))      ;pprint lepsa stampa, otvara nove redove
(()
 (0)
 (0 1)
 (0 1 2)
 (0 1 2 3)
 (0 1 2 3 4)
 (0 1 2 3 4 5)
 (0 1 2 3 4 5 6)
 (0 1 2 3 4 5 6 7)
 (0 1 2 3 4 5 6 7 8))
nil)

(defn neighbours  ;komsije tacke. Uzima/racuna sve pozicije (tacke) oko trenutne tacke koje nisu nil
  [[x y]]           ;trenutna tacka
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]  ;when je true samo kada su dx i dy nula. To je slucaj kada bi nova tacka imala
    (do                                                  ;iste koordinate kao i trenutna tacka pa je ne racunamo
      ;(println "dx dy" [x y] dx dy)
      [(+ dx x) (+ dy y)])))  ;vraca koordinate svih tacaka oko trenutne/donete/zive tacke





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Radi i bez ovoga
(defn count-neighbours           ;racunanje broja susednih polja
  [board loc]                    ;tabela i lokacija polja u tabeli-dolaze sva moguca polja [0 1],[0 2],...[3 4],[3 5]...[5 4][5 5]
  (let [brpolja (count (filter #(get-in board %) (neighbours loc)))]
    ;(println "brpolja" loc ":" brpolja)
    brpolja))   ;return funkcije count-neighbours
                                                 ;count-broj polja
                                                 ;filter izbacuje iz resenja sva polja kod kojih je ispunjen uslov #(get-in board)
                                                   ;get-in izdvaja iz tabele board vrednost polja sa koordinatama %
;PRVI NACIN                                                   
(defn indexed-step
  "Yields the next state of the board, using indices to determine neighbors,liveness, etc."
  [board]
  ;(println "-------------------------------------------NOVI POCETAK")
  ;(pprint board)
  (let [w (count board)                     ;w-broj redova (rows) - racuna se kao broj vektora u boardu
        h (count (first board))]            ;h-broj kolona (columns) - racuna se kao broj polja u prvom redu
    ;(println "cccc" w h)
    (loop [new-board board x 0 y 0]         ;formira new-board koji je vrste board i vrti od x=0 i y=0
      ;(println x y)
      (cond                                  ;switch (ali sa uslovom. kod switch mora da bude string, broj ili neka konstantna vrednost)
        (>= x w) new-board                   ;stampa new-board i izlazi iz loop petlje
        (>= y h) (recur new-board (inc x) 0) ;rekurzija za loop
                                                   ;new-board uzima trenutnu (svoju) vrednost, x=x+1, y=0
        :else
        ;ovo su pravila igre
        (let [new-liveness                     ;vrednost koja se upisuje u polje new-board
              (case (count-neighbours board [x y])    ;switch sa konstantom kao izborom.
                                                      ;pita koliko susednih polja ima trenutno polje (koja su :on)
                2 (get-in board [x y])                   ;ako je 2 ostaje kako je bilo. get-in izdvaja iz board (donete trenutno stare tabele)
                                                                  ;polje sa koordinatama [x y]
                3 :on                                    ;ako je 3 onda to polje postaje ili ostaje :on
                nil)]                                    ;u svim ostalim slucajevima je nil
          (recur (assoc-in new-board [x y] new-liveness) x (inc y))))))) ;rekurzija za loop
                                                                           ;u polje new-board upisuje dobijenu vrednost iz prethodnog let-a u koordinati [x y]
                                                                           ;x=x, y=y+1

;(-> (iterate indexed-step glider) (nth 3) pprint)         ;poziva indexed-step i vrti tri iteracije

;DRUGI NACIN
(defn indexed-step2
  [board]
  (let [w (count board)                     ;w-broj redova (rows) - racuna se kao broj vektora u boardu
        h (count (first board))]            ;h-broj kolona (columns) - racuna se kao broj polja u prvom redu
    (reduce                    ;spaja, dodaje na postojeci izlaz sledece ozracunate, umetnute vrednosti
      (fn [new-board1 x]           ;funkcija koja formira (new-board) za ulaze (x), tj. formira kolone u novoj tabeli
                                     ;new-board je w-vectora, h-podataka
                                     
        (reduce                    ;spaja, dodaje na postojece izlaze sledece obracunate, umetnute vrednosti
          (fn [new-board2 y]         ;funkcija koja formira (new-board) za ulaze (x), tj. formira kolone u novoj tabeli
                                        ;new-board2 je w-vectora, h-podataka tj. matrica
            ;ovo su pravila igre                                        
            (let [new-liveness                    ;vrednost koja se upisuje u polje new-board
                  (case (count-neighbours board [x y])
                    2 (get-in board [x y])                   ;ako je 2 ostaje kako je bilo. get-in izdvaja iz board (donete trenutno stare tabele)
                                                                  ;polje sa koordinatama [x y]
                    3 :on                                    ;ako je 3 onda to polje postaje ili ostaje :on
                    nil)]                                    ;u svim ostalim slucajevima je nil
              (assoc-in new-board2 [x y] new-liveness)));u polje new-board upisuje dobijenu vrednost iz prethodnog let-a u koordinati [x y]
          
          new-board1                            ;format izlaza za drugi reduce i pocetno stanje
          (range h)))                           ;vrednosti koje uzima y
      
      board                                ;format izlaza za prvi reduce (new-board) i pocetno stanje
      (range w))))                         ;vrednosti koje uzima ulaz (x)

;(-> (iterate indexed-step2 glider) (nth 3) pprint)         ;poziva indexed-step2 i vrti tri iteracije

;TRECI NACIN
(defn indexed-step3
  [board]
  (let [w (count board)                     ;w-broj redova (rows) - racuna se kao broj vektora u boardu
        h (count (first board))]            ;h-broj kolona (columns) - racuna se kao broj polja u prvom redu
    (reduce                      ;spaja, dodaje na postojece izlaze sledece obracunate, umetnute vrednosti
      (fn [new-board [x y]]         ;funkcija koja formira (new-board) za ulaze (x), tj. formira kolone u novoj tabeli
                                        ;new-board2 je w-vectora, h-podataka tj. matrica
        (let [new-liveness                    ;vrednost koja se upisuje u polje new-board
                           
              ;ovo su pravila igre              
              (case (count-neighbours board [x y])
                2 (get-in board [x y])                   ;ako je 2 ostaje kako je bilo. get-in izdvaja iz board (donete trenutno stare tabele)
                                                              ;polje sa koordinatama [x y]
                3 :on                                    ;ako je 3 onda to polje postaje ili ostaje :on
                nil)]                                    ;u svim ostalim slucajevima je nil
          (assoc-in new-board [x y] new-liveness)));u polje new-board upisuje dobijenu vrednost iz prethodnog let-a u koordinati [x y]
      
      board                                      ;format izlaza za prvi reduce (new-board) i pocetno stanje 
      (for [x (range h) y (range w)] [x y]))))   ;vrti ulazne vrednosti [x y] u vrenostima matrice (vektor vektora)

;(-> (iterate indexed-step3 glider) (nth 3) pprint)         ;poziva indexed-step3 i vrti tri iteracije
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Dovde radi bez ovoga

(into [](range 10 17))
;[10 11 12 13 14 15 16]

(partition 5 1 (range 9))            ;pravi liste od pet elemenata, gde su elementi range 9 tj. 0-8
                                     ;i svaki prvi element liste je uvecan za 1 u odnose na prethodni
;((0 1 2 3 4) (1 2 3 4 5) (2 3 4 5 6) (3 4 5 6 7) (4 5 6 7 8))

(partition 3 1 (concat [nil] (range 5) [nil]))   ;concat spaja nil sa 0 1 2 3 4 i nil. ovde su elementi nil 0 1 2 3 4 nil
                                                 ;pa se od njih prave trojke gde je svaki prvi clan biran sa stepom 1
;((nil 0 1) (0 1 2) (1 2 3) (2 3 4) (3 4 nil))

(defn window              ;222222222222222 DRUGI KORAK 
                          ;555555555555555 PETI KORAK
  "Returns a lazy sequence of 3-item windows centered around each item of coll, padded as necessary with pad or nil."
  ([coll] (window nil coll))  ;ako je dolaz samo jedna promenjiva (npr samo kolona). onda funkcija poziva samu sebe
                              ;sa novim parametrima, tj. deo funkcije sa dva parametra gde je prvi parametar nil
  ([pad coll]          ;ako su dolaz dve promenjive (npr [nil nil nil ...] i kolona)
    ;(println "----- kolone -----")
    ;(pprint coll)    ;u prvom prolazu coll je CELA TABELA pozvana iz index-free-step
                     ;u svakom sledecem prolazu coll je sest trojki vectora pozvanih iz cell-block
                     #_([[nil :on nil nil nil nil]
                         [nil nil :on nil nil nil]
                         [:on :on :on nil nil nil]
                         [nil nil nil nil nil nil]
                         [nil nil nil nil nil nil]
                         [nil nil nil nil nil nil]
                         [nil nil nil nil nil nil]])
                     ;concat dodaje ne ovu matricu jos na pocetku i na kraju jos jedan red sa po sest nil (u liniji ispod)
    ;(println "conc" (partition 3 1 (concat [pad] coll [pad])))                
    (partition 3 1 (concat [pad] coll [pad])))) ;pravi trojke gde se svaki prvi clan bira sa stepom 1
                                                ;u prvom prolazu to su sve trojke . svaki clan trojke ima sest clanova
                                                ;matrica ispod je ulaz za step-row. svaki red je jedan poseban ulaz
                                                #_(((nil nil nil nil nil nil) [nil :on nil nil nil nil] [nil nil :on nil nil nil]) 
                                                   ([nil :on nil nil nil nil] [nil nil :on nil nil nil] [:on :on :on nil nil nil]) 
                                                   ([nil nil :on nil nil nil] [:on :on :on nil nil nil] [nil nil nil nil nil nil]) 
                                                   ([:on :on :on nil nil nil] [nil nil nil nil nil nil] [nil nil nil nil nil nil]) 
                                                   ([nil nil nil nil nil nil] [nil nil nil nil nil nil] [nil nil nil nil nil nil]) 
                                                   ([nil nil nil nil nil nil] [nil nil nil nil nil nil] [nil nil nil nil nil nil]) 
                                                   ([nil nil nil nil nil nil] [nil nil nil nil nil nil] (nil nil nil nil nil nil)))
                                                
                                                ;u DRUGOM (i svim ostalim krugovima (dok se ne iscrpe svi redovi matrice)) KRUGU 
                                                ;kada poziva cell-block ulaz je sest vektora sa po tri clana
                                                ;([nil nil nil] [nil :on nil] [nil nil :on] [nil nil nil] [nil nil nil] [nil nil nil])
                                                ;izlaz je sest trojki vrktora sa po tri clana
                                                ;conc ((nil [nil nil nil] [nil :on nil]) 
                                                ;      ([nil nil nil] [nil :on nil] [nil nil :on]) 
                                                ;      ([nil :on nil] [nil nil :on] [nil nil nil]) 
                                                ;      ([nil nil :on] [nil nil nil] [nil nil nil]) 
                                                ;      ([nil nil nil] [nil nil nil] [nil nil nil]) 
                                                ;      ([nil nil nil] [nil nil nil] nil))
                                                ;svaka od ovih trojki je ulaz za liveness
                                                
(defn cell-block             ;CETVRTI KORAK KORAK
  "Creates a sequences of 3x3 windows from a triple of 3 sequences."
  [[left mid right]]
  ;(println "sledeci pozivi" [left mid right])
  ;(println "             " (map vector left mid right)) ;([nil nil nil] [nil :on nil] [nil nil :on] [nil nil nil] [nil nil nil] [nil nil nil])
  (window (map vector left mid right)))  ;map pravi list (vector-vectora [left mid right]) i to tako sto pravi trojke svih 
                                         ;prvih clanova pa zatim svih drugih clanova pa trecih ... pa n-tih. rezlultat je u liniji iznad
                                         ;window poziva funkciju sa jednim parametrom koja prvi parameter definise kao nil
                                         
(defn liveness              ;666666666666666 SESTI KORAK
  "Returns the liveness (nil or :on) of the center cell for the next step."
  [block]
  (let [[_ [_ center _] _] block]  ;izvaja vrednost centralnog (tekucek) polja po sistemu [[vector] [polje centar polje] [vector]]
    (case                              ;izbor
      (- (count (filter #{:on} (apply concat block)))  ;apply spaja u list sve vrednosti vektora u block
                                                       ;filter prociscava samo :on iz resenja prethodne linije
                                                       ;count broji broj calnova u filtriranom listu
         (if (= :on center) 1 0))               ;dobijeni broj se umanjuje za jedan ako je centrani clan :on
      2 center        ;ako je broj polja :on 2 onda je vrednost ne promenjena
      3 :on           ;ako je vresdnost 3 onda je nova vrednost polja :on
      nil)))          ;u svim ostalim slucajevima je nil

(defn step-row                    ; 3333333333333 TRECI KORAK
  "Yields the next state of the center row."
  [rows-triple]     ;ulaz su sest vectora trojki
  ;(println "rows-triple" rows-triple)  ;rows-triple ((nil nil nil nil nil nil) [nil :on nil nil nil nil] [nil nil :on nil nil nil])...
  (vec (map liveness (cell-block rows-triple)))) ;vec - pretvara listu u vector
                                                 ;map - pravi listu vectora tj u nasem slucaju pravi vector od sest elemenata
                                                 ;[:on nil :on nil nil nil]

;PROGRAMSKO RESENJE
(defn index-free-step             ;1111111111111111 PRVI KORAK
   "Yields the next state of the board."
   [board]
   ;(println  "------------------------------------POCETAK") (println "prvi poziv")
   (vec (map step-row (window (repeat 6 nil) board))) ;mora repeat nil da bi se napravio prvi nepostojeci red matrice sa svim nilovima
                                                      ;pravi novu matricu (sa pomerenim vrednostima on/nil pravila igre)
                                                      ;map pravi listu vectora
                                                      ;vec listu vektora pretvara u vektor vektora
                                                      ;[[nil nil nil nil nil nil] 
                                                      ; [:on nil :on nil nil nil] 
                                                      ; [nil :on :on nil nil nil] 
                                                      ; [nil :on nil nil nil nil] 
                                                      ; [nil nil nil nil nil nil] 
                                                      ; [nil nil nil nil nil nil] 
                                                      ; [nil nil nil nil nil nil]]
   
   
   ;(println "kraj prvog poziva" )                    ;poziva funkciju window sa parametrima [nil nil ... ] tabela
 )


#_(= (nth (iterate indexed-step glider) 1)          ;poziva PRVI NACIN 
         (nth (iterate index-free-step glider) 1))      ;poziva programsko resenje i rezultat je true - resenja su ista


;(partition 3 1 (concat [[nil nil nil nil nil nil]] [[nil :on nil nil nil nil]] [[nil nil :on nil nil nil]]))

(defn step
  "Yields the next state of the world"
  [cells]
  (println "------POCETAK NOVOG")
  (println "cells" cells)
  (set      ;u sustini ne treba     
   (for [[loc n] (frequencies (mapcat neighbours cells))  ;u varijablu loc upisuje koja je vrednos varijable
                                                          ;u varijablu n upisuje koliko se puta ponavlja vrednost varijable
                                                          ;i sve se to ponavlja dokle ima polja 
                                                          ;(za svako polje je definisano osam komsijskih polja)
                                                          ;neighbours - Uzima/racuna sve pozicije (tacke) oko trenutne tacke koje nisu nil
         :when (or (= n 3) (and (= n 2) (cells loc)))]  ;when se koristi kada je potreban samo true izbor
                                                        ;celija nastavlja ili pocinje da zivi samo ako ima tri suseda ili
                                                        ;ima dva suseda i bila je ziva u proslosti
     loc)) )  ;u resenje funkcije (ziva polja) ulaze samo polja gde je :when true

#_(->> (iterate step #{[2 0] [2 1] [2 2] [1 2] [0 1]})  ;polja koja su u startu :on. za svaku od njih se poziva funkcija step
   (drop 1)                 ;broj koraka (ako je 0 postavljaju se samo default vrednosti. ako je 1 poziva se funkcija jednom ...)
   first                         ;uzima praznu tabelu kreiranu u populate
   (populate (empty-board 6 6))    ;kreira praznu tabelu  ;ovo PRVO IZVRSAVA
   pprint)                         ;lepa stampa

(defn f1
  [n]
  [(- n 1) n (+ n 1)])
(f1 1)
;[0 1 2]
(map f1 [4 9 10])   ;svaku vrednost uvrstava u funkciju f1 i resenja pakuje u vektor koje pakuje u list
;([3 4 5] [8 9 10] [9 10 11])
(apply concat (map f1 [4 9 10]))  ;spoji sta ima sa ponavljanjem
;(3 4 5 8 9 10 9 10 11)
(mapcat f1 [4 9 10])       ;radi i sto sto i prethodna funkcija / SKRACENO od prethodne funkcije
;(3 4 5 8 9 10 9 10 11)
(frequencies (mapcat f1 [4 9 10]))
;{3 1, 4 1, 5 1, 8 1, 9 2, 10 2, 11 1}  trojka se ponavlja jednom, ..., devetka dva puta, ...

(defn stepper
   "Returns a step function for Life-like cell automata.
   neighbours takes a location and return a sequential collection
   of locations. survive? and birth? are predicates on the number
   of living neighbours."
   [neighbours birth? survive?]      ;birth? i survive? imaju znak pitanja samo sto su imena promenjivih tako definisana (lakse nam je da pratimo
                                             ;jer se koriste u pitalici). Sasvim je svedno i sa smo ih krstili bez znaka pitanja
   ;(println "iuiytt" neighbours birth? survive?)
   ;iuiytt #<core$hex_neighbours clojure0305ja.core$hex_neighbours@16a4b108> #{2} #{4 3)
   (fn [cells]     ;u cells su smestene vrednosti koje su donete u neighbours. cells su koordinate tacke
     ;cells #{[2 2] [2 0] [2 1] [1 2] [0 1]}
     ;(println "cells" cells) 
        ;cells #{[45 46] [45 47] [45 45] [46 46]}     
     ;(println (mapcat neighbours cells))
       ;([44 45] [44 46] [44 47] [45 45] [45 47] [46 45] [46 46] [46 47] [44 46] [44 47] [44 48] [45 46] [45 48] [46 46] [46 47] [46 48] [44 44] [44 45] [44 46] [45 44] [45 46] [46 44] [46 45] [46 46] [45 45] [45 46] [45 47] [46 45] [46 47] [47 45] [47 46] [47 47])       
     ;(println (frequencies (mapcat neighbours cells)))
        ;{[47 47] 1, [45 48] 1, [47 45] 1, [44 46] 3, [45 46] 3, [46 45] 3, [46 48] 1, [44 47] 2, [47 46] 1, [44 45] 2, [44 44] 1, [44 48] 1, [45 47] 2, [45 45] 2, [46 46] 3, [46 44] 1, [46 47] 3, [45 44] 1}
     (set (for [[loc n] (frequencies (mapcat neighbours cells))  ;u varijablu loc upisuje koja je vrednos varijable
                                                                 ;u varijablu n upisuje koliko se puta ponavlja vrednost varijable
                                                                 ;i sve se to ponavlja dokle ima tacaka definisanih u cells
                                                                 ;(za svako polje je definisano osam komsijskih polja)
                :when (if (cells loc) (survive? n) (birth? n))]  ;tacka ce biti ziva samo ako je :when ispunjno
                                                                 ;PRVO PROVERAVA if (cells loc)
                                                                 ;ako je taj if true onda proverava uslov (survive? n) ako je i on true belezimo rezultat u loc
                                                                 ;ako nije onda proverav uslov (birth? n) ako je on true sve ukupno je true ako nije ne belezi se resenje
                                                                    ;cells su koordinate trenutne zive tacke
                                                                    ;loc su koordinate svih (za sva ziva polja) susednih tacaka
                                                                    ;pitanje za ispunjen if uslov
                                                                    ;nema jednakosti zato sto su koordinate, da su brojevi mora biti znak =
                                                                 ;tj. AKO resenje if (cells loc) ima true resenje za 
                                                                 ;IF true - ako je broj suseda tacke 3 ili 4 donete vrednosti survive? je #{3 4}
                                                                 ;IF false - ako je broj susednih tacaka jednak 2 doneta vrednost birth? je #{2}
            loc))))       ;u resenja (tacke su zive) ulaze samo tacke kod kojih je ispunjen uslov :when 
(defn when_if
  [donos? d?]
  (fn [x] 
    (for [p (range 100)
          :when (if (= x p) (= d? p) (= donos? p))]
          (println p))))

;((when_if 2 3) 3)   ;poziva when_if sa paaametrima
                      ;donos? = 2
                      ;d? =3
                      ;parametar x podfunkcije = 3
#_(
2 3           ;resenja su (2 3)
3               ;2 za x(3)<> p(2) i donos?(2)=p(2) 
(2 3))          ;3 za x(3)=p(3) i d?(3)=p(3)    

(defn hex-neighbours          ;ulaz su sve zive tacke koje se nalaze u setu i za svaku zivu tacku racuna nove tacke koje ozivljavaju
                                ;u njenom komsiluku, tako da ako je u pitanju tekuca taka (dx=0) 
                                ;zbog ekspanzije se koordinate dy pomeraju za dva poena
  [[x y]]
  ;(println "--------------hh" [x y])
  #_(--------------hh [2 2]
     --------------hh [2 0]
     --------------hh [2 1]
     --------------hh [1 2]
     --------------hh [0 1]
     )
  (for [dx [-1 0 1] dy (if (zero? dx) [-2 2] [-1 1])] ;vrti dx sa vrednostima [-1 0 1] i dy tako da je dy [-2 2] ako je dx nula
                                                                                                       ;i [-1 1] ako dx nije nula
    (do
      ;(println "iiiiiiii" [x y] [dx dy] [(+ dx x) (+ dy y)])
      #_(iiiiiiii [2 2] [-1 -1] [1 1]
         iiiiiiii [2 2] [-1 1] [1 3]
         iiiiiiii [2 2] [0 -2] [2 0]
         iiiiiiii [2 2] [0 2] [2 4]
         iiiiiiii [2 2] [1 -1] [3 1]
         iiiiiiii [2 2] [1 1] [3 3])
      [(+ dx x) (+ dy y)])))
;(def hex-step (stepper hex-neighbours #{2} #{3 4}))  ;poziva stepper sa donetim zivim tackama i 
                                                     ;definisanim  birth? = 2 i survive? = 3,4
                                                     ;radja se nova ako ima dva suseda i opstaje ako ima tri ili cetiri suseda

;(hex-step #{[2 0] [2 1] [2 2] [1 2] [0 1]})   ;poziva hex-step sa zivim tackama i zive tacke smesta u hex-neighbours
;#{[2 3] [1 1] [0 3] [3 1]}



(defn rect-stepper 
  "Returns a step function for standard game of life on a (bounded) rectangular
   board of specified size."
  [w h]           ;w-sirina table, h-visina table
  (stepper #(filter (fn [[i j]] (and (< -1 i w) (< -1 j h)))   ;poziva funciju stepper 
                                                             ;filter kontrolise da li su tacke u okviru zadate forme.
                                                             ;tacke koje su van forme nece biti prikazane
                    ;(neighbours %)) #{2 3} #{1}))
                    (neighbours %)) #{2 3} #{3}))

(defn draw
  [w h step cells] ;w-sirina, h-visina, step-izlaz iz funkcije rect-stepper, cels-pocetne tacke koje su zive
  ;(println w h step cells)
  ;90 90 #<core$stepper$fn__11023 clojure0305ja.core$stepper$fn__11023@2faf4eb9> #{[45 46] [45 47] [45 45] [46 46]}
  (let [state (atom cells)          ;state je atom ge su smestene sve vrednosti zivih polja
        run (atom true)             ;atom za kontrolu kraja izvrsenja programa
        listener (proxy [java.awt.event.WindowAdapter] []
                   (windowClosing [_] (reset! run false)))  ;listener za prekid programa. kada se zatvori forma @run = false
        pane
          (doto (proxy [javax.swing.JPanel] []                                  ;formiranje panela
                  (paintComponent [^java.awt.Graphics g]                        
                    (let [g (doto ^java.awt.Graphics2D (.create g)              ;formiranje grafike
                              (.setColor java.awt.Color/BLACK)                  ;trenutna boja crna
                              (.fillRect 0 0 (* w 100) (* h 10))                ;crta pravougaonik sa pozadinom na formi
                              ;(.setColor java.awt.Color/WHITE)                  
           (.setColor (Color. 165 11 11 255))            ;SA (import'(java.awt Color))        ;boja zivih polja
                                                             ;rgb + oppacity. ako nema podrazumeva se 255 max pokrivanje
           ;(.setColor (java.awt.Color. 165 80 80 255))   ;BEZ (import'(java.awt Color))
           )]
                      (doseq [[x y] @state]           ;za svaku tacku iz @state crta se ziva tacka sa koordinatama ...
                        (.fillRect g (* 10 x) (* 10 y) 8 8)))))    ;nacrtaj zivu tacku
            (.setPreferredSize (java.awt.Dimension. (* 10 w) (* 10 h))))]    ;maksimalna velicina
    ;(println state)
    ;#<Atom@165af640: #{[45 46] [45 47] [45 45] [46 46]}>
    (doto (javax.swing.JFrame. "Quad Life")   ;Formira Frame sa Panelom pane
      (.setContentPane pane)          ; dodavanje panela pane
      (.addWindowListener listener)   ;dodavanje listenera listener
      .pack
      (.setVisible true))
    (future (while @run          ;ponavljaj dok je parametar @run = true
              (Thread/sleep 80)   ;usporava izvrsavanje - uspavljuje nit na 80 msec
              (swap! state step)  ;automatski svapuje (vrti sa dobijenim parametrima) podatke u atomu (state) 
              ;(println state)
              (.repaint pane)))))   ;osvezava panel

(defn rect-demo []
  (draw 90 90 (rect-stepper 90 90) 
      #{[45 45] [45 47] [46 46] [45 46]}))  ;definisanjepocetnih zivih tacaka

(rect-demo)





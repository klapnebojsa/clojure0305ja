(ns clojure0305ja.core
  (:use clojure.pprint))           ;koriscenje pprint komande

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
      ;(println dx dy)
      [(+ dx x) (+ dy y)])))





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


(= (nth (iterate indexed-step glider) 1)          ;poziva PRVI NACIN 
   (nth (iterate index-free-step glider) 1))      ;poziva programsko resenje i rezultat je true - resenja su ista


;(partition 3 1 (concat [[nil nil nil nil nil nil]] [[nil :on nil nil nil nil]] [[nil nil :on nil nil nil]]))












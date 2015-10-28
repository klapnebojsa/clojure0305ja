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
  (println "-------------------------------------------NOVI POCETAK")
  (pprint board)
  (let [w (count board)                     ;w-broj redova (rows) - racuna se kao broj vektora u boardu
        h (count (first board))]            ;h-broj kolona (columns) - racuna se kao broj polja u prvom redu
    ;(println "cccc" w h)
    (loop [new-board board x 0 y 0]         ;formira new-board koji je vrste board i vrti od x=0 i y=0
      (println x y)
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

(-> (iterate indexed-step3 glider) (nth 3) pprint)         ;poziva indexed-step3 i vrti tri iteracije
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Dovde radi bez ovoga









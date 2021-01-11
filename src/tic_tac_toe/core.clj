(ns tic-tac-toe.core)

;; GRA: TIC-TAC-TOE
;; Autor: Karolina Wadecka nr.albumu 36336

(def poczatkujaca-tablica
  "Podstawowa tablica, używana wtedy kiedy nowa gra się rozpoczyna"
  [1 2 3 4 5 6 7 8 9])


(defn trojki
  "Wszystkie linie w grze, które muszą być sprawdzone dla wygranego
   Bierze obecną tablicę jako argument"
  [tablica]
  (concat                                                   ;Utworzylismy jedna wielką kolekcję aby ustawić linie wygrywające :D
    (partition-all 3 tablica)                               ;Krawędzie tablicy
    (list
      (take-nth 3 tablica)                                  ;Pierwsza kolumna
      (take-nth 3 (drop 1 tablica))                         ;Druga kolumna
      (take-nth 3 (drop 2 tablica))                         ;Trzecia kolumna
      (take-nth 4 tablica)                                  ;Od lewej góry do prawego dołu przekątna
      (take-nth 2 (drop-last 2 (drop 2 tablica))))))        ;Od prawej góry do lewego dołu przekątna

(trojki poczatkujaca-tablica)
;; => ((1 2 3) (4 5 6) (7 8 9) (1 4 7) (2 5 8) (3 6 9) (1 5 9) (3 5 7))
;; ^ Różne kombinacje wygranych ^

(partition-all 3 poczatkujaca-tablica)
;; => ((1 2 3) (4 5 6) (7 8 9))
;; Tablica jest 3x3

(take-nth 3 poczatkujaca-tablica)
;; => (1 4 7)

(take-nth 4 poczatkujaca-tablica)
;; => (1 5 9)


(defn pelna-tablica?
  "Czy każda komórka jest wypełniona  :o lub :x?
  Bierze obecną tablicę jako argument"
  [tablica]
  (every? #{:x :o} tablica))

(defn wyswietlacz
  "Wyswietla status obecnej tablicy, przekazanej przez argument"
  [tablica]
   (let [tablica (map
                  #(if (keyword? %)                         ;; :x lub :o
                  (clojure.string/upper-case (name %))                                ;; lub (subs (str %) 1"<- "to jedna zastępcza metoda na zakrywanie słowa kluczowego stringiem" zamiana słowa kluczowego na stringa
                %)
                tablica)]
(println (nth tablica 0) (nth tablica 1) (nth tablica 2))
(println (nth tablica 3) (nth tablica 4) (nth tablica 5))
(println (nth tablica 6) (nth tablica 7) (nth tablica 8))))

;; Wyswietlacz wyswietla nam to
;; 1 x o
;; x o o
;; x o x

#_ (defn player-name
     "Przekonwertuje gracza słowa kluczowe na stringi z :x lub :o na o lub x"
     [gracz]
     (subs (str gracz) 1))

(name :x)
;; => "x"

(name :o)
;; => "o"

;; (clojure.string/upper-case (name %)) - Duże X i Duże O. Taki element stylistyczny :)
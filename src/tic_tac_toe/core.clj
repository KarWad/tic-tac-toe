(ns tic-tac-toe.core)

;; GRA: TIC-TAC-TOE
;; Autor: Karolina Wadecka nr.albumu 36336
;; skomentowałam kod więc wiadomo co do czego jest :D
;; MIŁEGO GRANIA!! <3


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TWORZENIE PODSTAW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(name :x)
;; => "x"

;; (clojure.string/upper-case (name %)) - Duże X i Duże O. Taki element stylistyczny :)

(defn nazwa-gracza
  "Przekonwertuje :o lub :x na stringa o lub x"
  [gracz]
  (clojure.string/upper-case (name gracz)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;WYKRYWANIE WYGRANEGO OSOBNIKA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn trojki-wygrany?
  "Jeżeli linia zawiera 3 x lub o, zwróć :x lub :o, jeżeli nie zawiera to zwróć 'nil'"
  [trojki]
  (if (every? #{:x} trojki)
    :x
    (if (every? #{:o} trojki)
      :o)))
;;testy czy funkcja działa
(trojki-wygrany? [1 2 3])
;; => nil
(trojki-wygrany? [:x 2 3])
;; => nil
(trojki-wygrany? [:x :x 5])
;; => nil
(trojki-wygrany? [:x :x :x])
;;=> :x

(trojki-wygrany? [:o 2 4])
;;=> nil
(trojki-wygrany? [:o :o 4])
;;=> nil
(trojki-wygrany? [:o :o :o])
;;=> :o

(defn wygrany?
  "Zwraca zwycięzce jeżeli taki jest, jak nie ma to zwraca 'nil'"
  [tablica]
  (first
    (filter #{:x :o} (map trojki-wygrany? (trojki tablica)))))

;;testy kodu
(wygrany? poczatkujaca-tablica)                             ;; czy na początku był jakis wygrany koles?
;;=> nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PODZIAŁ NA RUNDY W GRZE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sekwencja-gracza
  "Tworzymy sobie nieskończoną leniwą sekwencje dla turn gracza"
  (cycle [:o :x]))

(take 10 (cycle [:o :x]))
;;=> (:o :x :o :x :o :x :o :x :o :x)

(defn nastepny-ruch
  "Czyta nasze ruchy za pomoca 'read-line' i przetwarza je na inty
  Zwraca ruchy jeżeli wartosc istnieje na tablicy, jesli nie zwraca 'nil'"
  [tablica]
  (let [keyboard-input                                      ;; lokalna zmienna keyboard-input
        (try
          (. Integer parseInt (read-line))
          (catch Exception e nil))]
    (if (some #{keyboard-input} tablica)
      keyboard-input
      nil)))

(defn wez-runde
  "Pyta się gracza czy zrobi ruch i informuje go wtedy, kiedy gracz popełni błąd
  przy stawianiu kolejnego ruchu"
  [gracz tablica]
  (println (str (nazwa-gracza gracz) ":") "Zrob ruch (nacisnij numer pomiedzy 1-9 i nacisnij enter)"
           (loop [ruch (nastepny-ruch tablica)]
             (if ruch
                (assoc tablica (dec ruch) gracz))
(do
    (println (str (nazwa-gracza gracz) ":") "Mijesce w ktorym chcesz postawic znak jest juz zajete lub zle postawiles ruch,
    prosze abys zaznaczyl inne miejsce")
    (recur (nastepny-ruch tablica))))))

(defn graj
  "Pętla gry.
  Robimy alternatywne tury dla gracza dopuki ktos
  wygra albo tablica bedzie pelna"
  [poczatkujaca-tablica sekwencja-gracza]
  (loop [tablica poczatkujaca-tablica
         sekwencja-gracza sekwencja-gracza]
    (let [wygrany (wygrany? tablica)]
    (println "Zaktualizowana tablica:")
    (wyswietlacz tablica)
    (cond
      wygrany (println "Gracz " (nazwa-gracza wygrany) " wygrywa!!!")
      (pelna-tablica? tablica) (println "Jest remis!! :D")
      :else
      (recur
        (wez-runde (first sekwencja-gracza) tablica)
        (rest sekwencja-gracza))))))

(graj poczatkujaca-tablica sekwencja-gracza)
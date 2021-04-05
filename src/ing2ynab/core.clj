(ns ing2ynab.core
  (:require [clojure.string :as s])
  (:import [java.text NumberFormat]
           [java.util Locale]
           [java.time LocalDate]
           [java.time.format DateTimeFormatter])
  (:gen-class))

;; Converts ING export files with this format:
;; Buchung;Valuta;Auftraggeber/Empfänger;Buchungstext;Kategorie;Verwendungszweck;Saldo;Währung;Betrag;Währung
;; 
;; Om een of andere klote reden zijn uitreksels van onze gemeenschappelijke rekening anders geformateerd: de ;Katergorie kolom ontbreekt
;; 
;; Buchung;Valuta;Auftraggeber/Empfänger;Buchungstext;Verwendungszweck;Saldo;Währung;Betrag;Währung
;; 
;; To YNAB input files with this format
;; DATE,PAYEE,MEMO,OUTFLOW,INFLOW
;; 

(def ^:dynamic *header* "")

(defn convert-date [s]
  (let [in-formatter (DateTimeFormatter/ofPattern  "dd.MM.yyyy")
        out-formatter (DateTimeFormatter/ofPattern  "MM/dd/yyyy")
        parsed-date (LocalDate/parse s in-formatter)]
    (.format parsed-date out-formatter)))

(def parse-number
  (let [parser (doto
                (NumberFormat/getNumberInstance (Locale/GERMAN))
                 (.setParseBigDecimal true))]
    (fn [s]
      (let [parsed-number (.parse parser s)
            is-int? (= 0M (.remainder parsed-number BigDecimal/ONE))]
        (if is-int?
          ((if (pos? parsed-number) + -) parsed-number 0.01M)
          parsed-number)))))

(defn parse-line [l]
  (let [outflow-fn (fn [l] (let [v (parse-number (:Betrag l))] (if (< v 0M) (.abs v) 0M)))
        inflow-fn (fn [l] (let [v (parse-number (:Betrag l))] (if (> v 0M) (.abs v) 0M)))
        l (zipmap *header* (s/split l #";"))
        f (juxt (fn [l] (convert-date (:Valuta l))) ;; DATE
                (fn [l] (str "\"" (:Auftraggeber/Empfänger l) "\"")) ;; PAYEE
                (fn [l] (str "\"" (:Verwendungszweck l) "\"")) ;; MEMO
                outflow-fn ;; OUTFLOW
                inflow-fn)] ;; INFLOW
    (s/join "," (f l))))

(defn read-transform-print-loop []
  (let [i (read-line)]
    (when-not (nil? i) ;; Some bizar implementation logic makes EOF nil BUT trueisch
      (println (parse-line i))
      (flush)
      (recur))))

;; input file is encoded in ISO 8859-3
(defn -main
  "CAll with 
   iconv -f ISO8859-3 -t UTF8 < resources/ing.csv | tail -n +16 | java -jar target/uberjar/ing2ynab-0.1.0-SNAPSHOT-standalone.jar "
  [& args]
  (println "DATE,PAYEE,MEMO,OUTFLOW,INFLOW")
  (binding  [*header* (map keyword (s/split (read-line) #";"))]
    (read-transform-print-loop)))
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
;; To YNAB input files with this format
;; DATE,PAYEE,MEMO,OUTFLOW,INFLOW

(defn convert-date [s]
  (let [in-formatter (DateTimeFormatter/ofPattern  "dd.MM.yyyy")
        out-formatter (DateTimeFormatter/ofPattern  "MM/dd/yyyy")
        parsed-date (LocalDate/parse s in-formatter)]
    (.format parsed-date out-formatter)))

(defn parse-number [s]
  (let [parser (doto
                (NumberFormat/getNumberInstance (Locale/GERMAN))
                 (.setParseBigDecimal true))
        parsed-number (.parse parser s)
        fract-part (.remainder parsed-number BigDecimal/ONE)]
    (if (= fract-part 0M)
      ((if (pos? (.signum parsed-number)) + -) parsed-number 0.01M)
      parsed-number)))

(defn parse-line [l]
  (let [outflow-fn (fn [l] (let [v (parse-number (nth l 8))] (if (< v 0M) (.abs v) 0M)))
        inflow-fn (fn [l] (let [v (parse-number (nth l 8))] (if (> v 0M) (.abs v) 0M)))
        f (juxt (fn [l] (convert-date (nth l 1))) ;; DATE
                (fn [l] (str "\"" (nth l 2) "\"")) ;; PAYEE
                (fn [l] (str "\"" (nth l 5) "\"")) ;; MEMO
                outflow-fn ;; OUTFLOW
                inflow-fn)] ;; INFLOW
    (-> l (s/split #";") f (as-> a (s/join "," a)))))

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
  (read-transform-print-loop))
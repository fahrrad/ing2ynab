(ns ing2ynab.core-test
  (:require [clojure.test :as t]
            [ing2ynab.core :as sut]))

(t/deftest parse-line-test
  (t/testing "Maps 1 line correcty with inflow"
    (let [ing-line "18.02.2021;18.02.2021;COMPANY GMBH;Lastschrift;Lebensmittel und Haushalt;NR XXXX BERLIN KAUFUMSATZ 16.02 204736 ARN74438671234123434049843 Apple Pay;713,03;EUR;-50,49;EUR"
          ynab-line "02/18/2021,\"COMPANY GMBH\",\"NR XXXX BERLIN KAUFUMSATZ 16.02 204736 ARN74438671234123434049843 Apple Pay\",50.49,0"
          result (sut/parse-line ing-line)]
      (t/is (= ynab-line result))))
           (t/testing "Maps 1 line correcty with outflow"
    (let [ing-line "18.02.2021;18.02.2021;COMPANY GMBH;Lastschrift;Lebensmittel und Haushalt;NR XXXX BERLIN KAUFUMSATZ 16.02 204736 ARN74438671234123434049843 Apple Pay;713,03;EUR;50,49;EUR"
          ynab-line "02/18/2021,\"COMPANY GMBH\",\"NR XXXX BERLIN KAUFUMSATZ 16.02 204736 ARN74438671234123434049843 Apple Pay\",0,50.49"
          result (sut/parse-line ing-line)]
      (t/is (= ynab-line result)))))

(t/deftest convert-date-test
  (t/testing "convert date in German format to US"
    (t/is (= "02/18/2021" (sut/convert-date "18.02.2021")))))

(t/deftest parse-number-test
  (t/testing "to work around a bug in ynab, add 1 cent to round numbers"
    (t/is (= 10.01M (sut/parse-number "10,00")))
    (t/is (= -10.01M (sut/parse-number "-10,00"))))
  (t/testing "Parse numbers in German format"
    (t/is (= 45.54M (sut/parse-number "45,54")))
    (t/is (= -45.54M (sut/parse-number "-45,54")))))

(t/deftest parse-csv-test
  (let [test-csv ["h1;h2;h3" "1;2;3" "2;4;6"]]
    (t/testing "getting a list of maps"
      (let [result (sut/parse-csv test-csv)]
        (t/is (= '({:h1 "1" :h2 "2" :h3 "3"}
                   {:h1 "2" :h2 "4" :h3 "6"}) result))))))

(comment 
  (seq? [:a]))
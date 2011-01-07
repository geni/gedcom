(ns clj-gedcom.core-test
  (:use clojure.test)
  (:use clj-gedcom.core))

(deftest parser

  (testing "gedcom-line"
    (are [input expected] (= expected (into {} (gedcom-line input)))
      "0 FOO"           {:level "0" :tag "FOO"}
      "0 @I1@ FOO data" {:level "0" :label "@I1@" :tag "FOO" :data "data"}
      "0 FOO data"      {:level "0" :label nil :tag "FOO" :data "data"}
      "0 FOO @I1@"      {:level "0" :label nil :tag "FOO" :data "@I1@"}
    )))


;    (apply (is (= %2 (gedcom-line %1)) inputs-to-outputs-map))))


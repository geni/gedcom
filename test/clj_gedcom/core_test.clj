(ns clj-gedcom.core-test
  (:use [clojure.string :only [split-lines]])
  (:use clojure.test)
  (:use clj-gedcom.core))

(deftest parser
  (testing "gedcom-line"
    (are [input expected] (= expected (into {} (gedcom-line input)))
      "0 FOO"           {:level 0 :label nil :tag "FOO" :data nil}
      "0 @I1@ FOO data" {:level 0 :label "@I1@" :tag "FOO" :data "data"}
      "0 FOO data"      {:level 0 :label nil :tag "FOO" :data "data"}
      "0 FOO @I1@"      {:level 0 :label nil :tag "FOO" :data "@I1@"}))

  (testing "gedcom-line-seq"
    (are [input expected]
         (= expected (into {} (->> input (map gedcom-line) gedcom-line-seq first))
      "0 FOO one\n1 CONT two\n1 CONC  three" {:level 0 :label nil :tag "FOO" :data "one\ntwo three"}))))

(ns clj-gedcom.core
  (:use [clojure.java.io :only [reader]]))

(defrecord GedcomLine [level label tag data])

(defn gedcom-line [line]
  (when line
    (let [[_ level label tag data] (re-matches #"^\s*(\d)\s+(@[^@]+@)?\s*(\w+)\s+(.*)?$" line)]
      (GedcomLine. level label tag data))))

(defn gedcom-line-seq
  "Read a GEDCOM line from a line sequence returning a gedcom-line.
  Lines can be in the following formats:
    level label tag data
    level tag data
    level tag data\\r(level+1) CONT data
    level tag data\\r(level+1) CONC data"
  [line-seq]
  (lazy-seq
    (loop [line-seq (rest line-seq)
           line     (gedcom-line (first line-seq))]
      (when line
        (let [{:keys [tag data]} (gedcom-line (first line-seq))]
          (if (contains? #{"CONT" "CONC"} tag)
            (recur (rest line-seq)
                   (update-in line [:data]
                      #(str % (when (= "CONT" tag) "\n") data)))
            (cons line (gedcom-line-seq line-seq))))))))

(defn read-gedcom-record
  "Parse a GEDCOM record from a sequence returning a hash of the parts.
  "
  [seq]

)


(ns clj-gedcom.core
  (:use [clojure.java.io :only [reader]]))

; helper accessor (may need to rename)
(defn get-in* [r keys]
  (get-in r (interpose 0 keys)))

(defrecord GedcomLine [level label tag data])

(defn gedcom-line
  "Parse a GedcomLine record from a string."
  [line]
  (when line
    (let [[_ level label tag data] (re-matches #"^\s*(\d)(?:\s(@[^@]+@))?\s(\w+)(?:\s(.*))?$" line)]
      (GedcomLine. (Integer. level) label tag data))))

(defn gedcom-line-seq
  "Read a GEDCOM line from a line sequence returning a GedcomLine record.
  Lines can be in the following formats:
    level label tag data
    level tag data
    level tag data\\r(level+1) CONT data
    level tag data\\r(level+1) CONC data"
  [lines]
  (lazy-seq
   (loop [line  (first lines)
          lines (rest  lines)]
     (when line
       (let [{:keys [tag data]} (first lines)]
         (if (contains? #{"CONT" "CONC"} tag)
           (recur (update-in line [:data] str
                          (when (= "CONT" tag) "\n") data)
                  (rest lines))
           (cons line (gedcom-line-seq lines))))))))

(defn level< [& args]
  (apply < (map :level args)))

(defn parse-gedcom-record
  "Recursively parse a record and all records beneath it."
  [parent gedcom-lines]
  (let [line (first gedcom-lines)]
    (if (and line (level< parent line))
      (let [tail (drop-while (partial level< line) (rest gedcom-lines))]
        (parse-gedcom-record
          (update-in parent [(:tag line)] (fnil conj [])
                  (parse-gedcom-record line (rest gedcom-lines)))
          tail))
      parent)))

(defn gedcom-record-seq
  "Parse a GEDCOM record from a sequence returning a lazy-seq of hashes."
  [gedcom-lines]
  (lazy-seq
   (if (seq (rest gedcom-lines))
     (let [[head tail] (split-with (comp pos? :level) (rest gedcom-lines))]
       (cons (parse-gedcom-record (first gedcom-lines) head)
             (gedcom-record-seq tail)))
     (take 1 gedcom-lines))))

(defn parse-gedcom-records
  "Parses GEDCOM records from a file or reader, returning a seq of records."
  [in]
  (->> in reader line-seq (map gedcom-line) gedcom-line-seq gedcom-record-seq))

(defn parse-gedcom
  "Parses GEDCOM records from a file or reader, returning map of labels to records."
  [in]
  (reduce (fn [records record]
            (assoc records (or (:label record) (:tag record)) record))
          {}
          (parse-gedcom-records in)))
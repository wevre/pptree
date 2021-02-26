(ns wevre.pptree.main
  (:require [wevre.pptree.tree :as t]
            [wevre.pptree.print :as p]
            [wevre.pptree.cli :as cli]
            [wevre.natural-compare :refer [natural-compare]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.walk :as w])
  (:gen-class))

(def ^:dynamic *case-sens* false)
(def ^:dynamic *dirs-first* false)
(def ^:dynamic *dirs-last* false)
(def ^:dynamic *lex-sort* false)

(defn sort-childs
  "Sorts children by root, optionally (1) ignoring case, (2) placing dirs 
   first or last, and (3) sorting lexically (as opposed to naturally)."
  [childs]
  (let [filter-ci (fn [p] (if *case-sens* p (str/lower-case p)))
        dir-sorter (fn [dpre fpre] (fn [p] (str (if (t/dir? p) dpre fpre) p)))
        sort-dir (cond
                   *dirs-first* (dir-sorter "0" "1")
                   *dirs-last* (dir-sorter "1" "0")
                   :else identity)
        key-fn (comp filter-ci sort-dir :root)
        comp (if *lex-sort* compare natural-compare)]
    (sort-by key-fn comp childs)))

(defn tree<-input
  "Generates tree from input lines, with sorted children."
  [input]
  (->> input
       (map t/node<-path)
       (reduce t/add-node)
       (w/postwalk
        #(if (and (map? %) (:childs %)) (update % :childs sort-childs) %))))

(defn unwrap-quotes [s]
  (if (and (str/starts-with? s "\"") (str/ends-with? s "\""))
    (subs s 1 (max 1 (dec (count s))))
    s))

(defn -main [& args]
  (let [{:keys [:exit-message :ok? :options :separator]} (cli/validate-args args)]
    (if exit-message
      (cli/exit (if ok? 0 1) exit-message)
      (binding [t/*sep* (or separator t/*sep*)
                *case-sens* (get options :case-sensitive *case-sens*)
                *dirs-first* (get options :folders-first *dirs-first*)
                *dirs-last* (get options :folders-last *dirs-last*)
                *lex-sort* (get options :lexical-sort *lex-sort*)]
        (with-open [rdr (io/reader *in*)]
          (let [lines (->> (line-seq rdr)
                           (map str/trim)
                           (map unwrap-quotes)
                           (filter seq))]
            (when (seq lines)
              (doseq [x (->> lines tree<-input p/lines<-tree)]
                (println x)))))))))

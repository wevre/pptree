(ns wevre.pptree.print
  "Methods for converting a tree into lines with line-drawing prefixes"
  (:require [clojure.string :as str]
            [wevre.pptree.tree :as t]
            [clojure.zip :as z]))

(def spa "    ")
(def bra "│   ")
(def tee "├── ")
(def lst "└── ")

(defn lasts<-loc
  "Return seq of true/false, one for branch and each of its parents, indicating
   if the branch at that level is a last child."
  [loc]
  (let [ups (->> (iterate z/up loc) (take-while (complement nil?)))
        depth (dec (count ups))
        lasts (map #(nil? (z/right %)) ups)]
    (cond
      (not (t/dir? (z/node loc))) (take depth lasts)
      (< 1 depth) (take (dec depth) (drop 1 lasts))
      :else ())))

(defn prefixes<-lasts 
  "Convert list of boolean lasts into prefixes. First one belongs to node, so it
   is a corner or a tee, for subsequent parents, its a branch line or spacer."
  [lasts]
  (map-indexed (fn [i l] (if (zero? i) (if l lst tee) (if l spa bra))) lasts))

(defn lines<-tree
  "Return seq of printable lines from given tree."
  [tree]
  (let [get-line (fn [loc] (str (->> (lasts<-loc loc)
                                     prefixes<-lasts
                                     reverse
                                     str/join)
                                (:root (z/node loc))))]
    (->> (t/zipper<-tree tree)
         (iterate z/next)
         (take-while (complement z/end?))
         (map get-line))))

(comment
  (let [input ["/h/a" "/h/a/b" "/h/a/b/c" "/h/d"]
        tree (->> input (map t/node<-path) (reduce t/add-node))]
    (doseq [x (lines<-tree tree)]
      (println x)))
  ;
  )
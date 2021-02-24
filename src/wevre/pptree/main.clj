(ns wevre.pptree.main
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [wevre.pptree.cli :as cli]
            [wevre.natural-compare :refer [natural-compare]]
            [clojure.walk :as w]
            [clojure.zip :as z])
  (:gen-class))

(def ^:dynamic *sep* "/")
(def ^:dynamic *case-sens* false)
(def ^:dynamic *dirs-first* false)
(def ^:dynamic *dirs-last* false)
(def ^:dynamic *lex-sort* false)

(defn split-last
  "Split path at final *sep*."
  [s]
  (if-let [i (str/last-index-of s *sep*)]
    (map str/join (split-at (inc i) s))
    (list "" s)))

(defprotocol Prefixed
  "Items (paths or trees) that contain removable prefix."
  (deprefix [val pfx] "Remove prefix")
  (root [val] "Path at root of item")
  (children [val])
  (as-tree [val] "Wrap item into seq if necessary")
  (as-child [val] "Unwrap 1-element seqs")
  (dir? [val] "True if item is a directory")
  (split [val] "Split path if it is not a directory")
  (sort-childs [val f] "Sort children with f"))

(extend-protocol Prefixed
  java.lang.String
   (deprefix [val pfx] (str/replace-first val pfx ""))
   (root [val] val)
   (children [val] ())
   (as-tree [val] (list val))
   (as-child [val] val)
   (dir? [val] (str/ends-with? val *sep*))
   (split [val] 
     (let [[head tail] (split-last val)] 
       (if (and (seq head) (seq tail)) (list head tail) val)))
   (sort-childs [val f] val)
  
  clojure.lang.Seqable
   (deprefix [val pfx] (cons (str/replace-first (first val) pfx "") (rest val)))
   (root [val] (first val))
   (children [val] (rest val))
   (as-tree [val] val)
   (as-child [val] (if (= 1 (count val)) (first val) val))
   (dir? [val] (or (< 0 (count val)) (dir? (first val))))
   (split [val] val)
   (sort-childs [val f] (if (< 1 (count val)) (cons (first val) (f (rest val))) val))
  
  nil
   (deprefix [val pfx] nil)
   (root [val] nil)
   (children [val] nil)
   (as-tree [val] nil)
   (as-child [val] nil)
   (dir? [val] nil)
   (split [val] nil)
   (sort-childs [val f] nil))

(defn common-prefix
  "Return common prefix ending with *sep*."
  [a b]
  (->> (map vector (root a) (root b))
       (take-while (partial apply =))
       (map first)
       (apply str)
       split-last
       root))

(defn re-root
  "Root tree and path under new, common-prefix parent."
  [tree path pfx]
  (cons pfx (->> (list tree path)
                 (map #(deprefix % pfx))
                 (map as-child)
                 (sort-by root))))

(defn add-path
  "Return tree with path added."
  [tree path]
  (letfn [(insert-down
           [[par & chs :as tree] path]
           (let [pfx (common-prefix par path)]
             #_(prn ["down" par chs tree path pfx])
             (cond
               (empty? tree) (as-tree path)
               (pos? (compare par pfx)) (re-root tree path pfx)
               (= path pfx) tree
               :else (cons par (insert-across chs (deprefix path pfx))))))
          (insert-across
           [[ch & chs :as childs] path]
           (let [pfx (common-prefix ch path)]
             #_(prn ["across" ch chs tree path pfx])
             (cond
               (nil? ch) (list path)
               (seq pfx) (cons (insert-down (as-tree ch) path) chs)
               (neg? (compare (root path) (root ch))) (cons path childs)
               (= (root path) (root ch)) childs
               :else (cons ch (insert-across chs path)))))]
    (insert-down tree path)))

(comment
  (reduce add-path () ["/hello/alpha" "/charlie"])
  (add-path ["/h/w"] "/h/w/d")
  (add-path ["/h/"] "/h/a")
  (add-path '("/l/" ("h/a/" "c")) "/l/h/b")
  (reduce add-path () ["/l/" "/l/h/a" "/l/h/b"])
  (add-path '("/h/" "a" ("b/" "c")) "/h/b/d")
  (add-path '("/h/c/") "/h/c/")
  (add-path '("/h/" ("a/b")) "/h/a/c")
  ;
  )

(def spa "    ")
(def bra "│   ")
(def tee "├── ")
(def lst "└── ")

(defn sort-chs
  "Sorts children by root, optionally (1) ignoring case, (2) placing dirs 
   first or last, and (3) sorting lexically (as opposed to naturally)."
  [chs]
  (let [filter-ci (fn [p] (if *case-sens* p (str/lower-case p)))
        dir-sorter (fn [dpre fpre] (fn [p] (str (if (dir? p) dpre fpre) p)))
        sort-dir (cond
                   *dirs-first* (dir-sorter "0" "1")
                   *dirs-last* (dir-sorter "1" "0")
                   :else identity)
        key-fn (comp filter-ci sort-dir root)
        comp (if *lex-sort* compare natural-compare)]
    (sort-by key-fn comp chs)))

(defn prefixes<-lasts [lasts]
  (map-indexed (fn [i l] (if (zero? i) (if l lst tee) (if l spa bra))) lasts))

(defn lines<-tree
  "Return seq of printable lines from given tree. Sort or split Tree should be 
   sorted and any lone directories split."
  [tree]
  (let [lasts<-loc   ; Seq of bools if each of branch+parents is "last".
        (fn [loc]
          (let [ups (->> (iterate z/up loc) (take-while (complement nil?)))
                depth (dec (count ups))
                lasts (map #(nil? (z/right %)) ups)]
            (cond
              (not (dir? (z/node loc))) (take depth lasts)
              (< 1 depth) (take (dec depth) (drop 1 lasts))
              :else ())))
        get-line (fn [loc] (str (->> (lasts<-loc loc)
                                     prefixes<-lasts
                                     reverse
                                     str/join) 
                                (z/node loc)))]
    (->> (z/seq-zip tree)
         (iterate z/next)
         (take-while (complement z/end?))
         (filter (complement z/branch?))
         (map get-line))))

(comment
  (let [input ["/h/a" "/h/a/b" "/h/a/b/c" "/h/d"]
        tree (->> (reduce add-path [] input)
                  (w/postwalk split)
                  (w/postwalk #(sort-childs % sort-chs)))]
    (lines<-tree tree))
  (lines<-tree nil)
  ;
  )

(comment
  (let [input ["/h/a" "/h/a/b" "/h/a/b/c" "/h/d"]
        tree (->> (reduce add-path [] input)
                  (w/postwalk split)
                  (w/postwalk #(sort-childs % sort-chs)))
        zipper (z/seq-zip tree)
        zseq (take-while (complement z/end?) (iterate z/next zipper))
        up-count (fn [loc] (count (take-while (complement nil?) (drop 1 (iterate z/up loc)))))
        last? (fn [loc] (nil? (->> loc z/right)))
        up-lasts (fn [loc] (map last? (take-while (complement nil?) (iterate z/up loc))))
        lasts (fn [loc]
                (let [depth (up-count loc)]
                  (cond
                    (not (dir? (z/node loc))) (take depth (up-lasts loc))
                    (< 1 depth) (take (dec depth) (drop 1 (up-lasts loc)))
                    :else ())))
        prefixes (fn [loc] (reverse (prefixes<-lasts (lasts loc))))]
    (prn tree)
    (map (juxt z/node prefixes) (filter (complement z/branch?) zseq)))
  ;
  )

(defn tree<-input [inp]
  (->> (reduce add-path () inp) 
       (w/postwalk split) 
       (w/postwalk #(sort-childs % sort-chs))
       seq))

(defn unwrap-quotes [s]
  (if (and (str/starts-with? s "\"") (str/ends-with? s "\""))
    (subs s 1 (max 1 (dec (count s))))
    s))

(defn -main [& args]
  (let [{:keys [:exit-message :ok? :options :separator]} (cli/validate-args args)]
    (if exit-message
      (cli/exit (if ok? 0 1) exit-message)
      (binding [*sep* (or separator *sep*)
                *case-sens* (get options :case-sensitive *case-sens*)
                *dirs-first* (get options :folders-first *dirs-first*)
                *dirs-last* (get options :folders-last *dirs-last*)
                *lex-sort* (get options :lexical-sort *lex-sort*)]
        (with-open [rdr (io/reader *in*)]
          (doseq [x (->> (line-seq rdr)
                         (map str/trim)
                         (map unwrap-quotes)
                         (filter seq)
                         tree<-input
                         lines<-tree)]
            (println x)))))))

(comment
  ;; Not sure why, but without seq we get "()"
  (let [input ""] (doseq [x (->> input tree<-input     lines<-tree)] (println x)))
  (let [input ""] (doseq [x (->> input tree<-input seq lines<-tree)] (println x)))
  ;; Note: moved seq to end of tree<-input
  ;
  )
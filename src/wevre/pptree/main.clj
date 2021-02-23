(ns wevre.pptree.main
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [wevre.pptree.cli :as cli]
            [wevre.natural-compare :refer [natural-compare]]
            [clojure.walk :as w])
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

(defn common-prefix
  "Return common prefix ending with *sep*."
  [a b]
  (->> (map vector a b)
       (take-while (partial apply =))
       (map first)
       (apply str)
       split-last
       first))

(defprotocol Prefixed
  "Items (paths or trees) that contain a removable prefix."
  (deprefix [val pfx]))

(extend-protocol Prefixed
  java.lang.String
  (deprefix [val pfx] (str/replace-first val pfx ""))
  clojure.lang.PersistentVector
  (deprefix [val pfx] (update val 0 #(str/replace-first % pfx ""))))

(defn add-path
  "Recursively add str `path` to vec `tree` and return new tree vector.
   Paths must be added in sorted order."
  [[par & _ :as tree] path]
  (let [pfx (common-prefix par path), de-path (deprefix path pfx)]
    (cond
      ; Initialize empty tree.
      (empty? tree) [path]
      ; Place par and path as siblings under new parent.
      (pos? (compare par pfx)) [pfx (deprefix tree pfx) [de-path]]
      ; Ignore duplicate directories.
      (= path pfx) tree
      ; Add path as last child.
      (or (= 1 (count tree)) 
          (empty? (common-prefix (first (peek tree)) de-path)))
      (conj tree [de-path])
      ; Recursively add path to last child.
      :else
      (conj (pop tree) (add-path (peek tree) de-path)))))

(def spa "    ")
(def bra "│   ")
(def tee "├── ")
(def lst "└── ")

(defn dir? [[par & chs]]
  (or (seq chs) (and par (str/ends-with? par *sep*))))

(defn sort-chs
  "Sorts children using each one's first entry, optionally (1) ignoring  
   case and (2) placing dirs first or last, if requested."
  [chs]
  (let [filter-ci (fn [ch] (if *case-sens* ch (update ch 0 str/lower-case)))
        dir-sorter
        (fn [dpre fpre]
          (fn [ch] (update ch 0 #(str (if (dir? ch) dpre fpre) %))))
        sort-dir (cond
                   *dirs-first* (dir-sorter "0" "1")
                   *dirs-last* (dir-sorter "1" "0")
                   :else identity)]
    (sort-by (comp first filter-ci sort-dir) 
             (if *lex-sort* compare natural-compare) 
             chs)))

(defn split-lone-ch
  "Splits root-only tree if not a directory."
  [[par & chs :as tree]]
  (let [[head tail] (split-last par)]
    (if (and (empty? chs) (seq head) (seq tail)) [head [tail]] tree)))

(defn unwind-prefix [[s p & d]]
  (str (str/join (map #(if % spa bra) (reverse d))) 
       (when-not (nil? p) (if p lst tee)) 
       s))

(defn get-prefixes
  "Transforms nested vector tree into list of paths+prefixes flags.
   Prefixes are in reverse order (built up with `conj`) and have to be reversed
   when unwound."
  [stack [par & chs]]
  (when par
    (let [last? (fn [i] (= i (dec (count chs))))]
      (cons (conj stack par)
            (mapcat (fn [c i] (get-prefixes (conj stack (last? i)) c)) 
                    (sort-chs chs)
                    (range))))))

(defn lines<-tree
  [tree]
  (->> tree
       (w/postwalk #(if (vector? %) (split-lone-ch %) %))
       (get-prefixes ()) 
       (map unwind-prefix)))

(comment
  (let [input ["/h/l/a" "/h/l/b" "/h/l/c" "/h/w/p" "/h/w/q" "/h/w/q/r" "/h/w/q/s" "/h/w/y"]]
    (->> input sort (reduce add-path []) lines<-tree))
  (let [input ["/h/a" "/h/a/b" "/h/a/b/c"]]
    (->> input sort (reduce add-path []) lines<-tree))
  
  (let [input ["/hello/alpha/charlie/file2.txt"
                 "/hello/alpha/charlie/file10.txt"
                 "/hello/alpha/Epsilon"
                 "/hello/alpha/delta"
                 "/hello/alpha/bravo"
                 "/hello/alpha/charlie/file1.txt"]]
    (->> input sort (reduce add-path []) lines<-tree)))

(defn unwrap-quotes [s]
  (if (and (str/starts-with? s "\"") (str/ends-with? s "\""))
    (subs s 1 (max 1 (dec (count s))))
    s))

(defn -main [& args]
  (let [{:keys [::cli/exit-message ::cli/ok? ::cli/options ::cli/separator]} (cli/validate-args args)]
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
                         sort
                         (reduce add-path [])
                         lines<-tree)]
            (println (str/join x))))))))

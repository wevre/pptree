(ns wevre.pptree.main
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [wevre.pptree.cli :as cli]
            [wevre.natural-compare :refer [natural-compare]])
  (:gen-class))

(def ^:dynamic *sep* "/")
(def ^:dynamic *case-sens* false)
(def ^:dynamic *dirs-first* false)
(def ^:dynamic *dirs-last* false)
(def ^:dynamic *lex-sort* false)

(defn split-last 
  "Split a path at final sep."
  [s]
  (if-let [i (str/last-index-of s *sep*)]
    (map str/join (split-at (inc i) s))
    (list "" s)))

(defn get-prefix
  "Return common prefix ending with sep."
  [a b]
  (->> (map vector a b)
       (take-while (partial apply =))
       (map first)
       (apply str)
       (split-last)
       (first)))

(defprotocol Prefixed
  "Items (paths or trees) that contain a prefix."
  (deprefix [val pfx]))

(extend-protocol Prefixed
  java.lang.String
  (deprefix [val pfx] (str/replace-first val pfx ""))
  clojure.lang.PersistentVector
  (deprefix [val pfx] (update val 0 #(str/replace-first % pfx ""))))

(defn add-path
  "Add str `path` to vec `tree` and return a new tree.
   Paths must be added in sorted order. Uses recursion."
  [[par & _ :as tree] path]
  (let [pfx (get-prefix par path) de-path (deprefix path pfx)]
    (cond
      (empty? tree) [path]
      (pos? (compare par pfx)) [pfx (deprefix tree pfx) [de-path]]
      (= path pfx) tree   ;; Ignore duplicate directories.
      (or (= 1 (count tree)) (empty? (get-prefix (first (peek tree)) de-path)))
      (conj tree [de-path])   ;; Add path as (newest) last child.
      :else   ;; Recursively add path to (existing) last child.
      (conj (pop tree) (add-path (peek tree) de-path)))))

(defn add-path*
  "Add str `path` to vec `tree` and return a new tree.
   Paths must be added in sorted order. Uses loop/recur."
  [tree path]
  (->> 
   ;; `pars` is an 'inside-out' stack of trees without their youngest child.
   ;; When the loop bottoms out, we'll recombine the first item in `pars` as the
   ;; last child of the second item, that will become the last child of the 
   ;; third and so on.
   (loop [[par & _ :as tree] tree pars [] path path]
     (let [pfx (get-prefix par path) de-path (deprefix path pfx)]
       (cond
         (empty? tree) (cons [path] pars)
         (pos? (compare par pfx)) (cons [pfx (deprefix tree pfx) [de-path]] pars)
         (= path pfx) (cons tree pars)   ;; Ignore duplicate directories.
         (or (= 1 (count tree)) (empty? (get-prefix (first (peek tree)) de-path)))
         (cons (conj tree [de-path]) pars)   ;; Add path as (newest) last child.
         :else   ;; Add path to (existing) last child.
         (recur (peek tree) (cons (pop tree) pars) de-path))))
    (reduce #(conj %2 %1))))

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
          (fn [ch] (update ch 0 #(str/join [(if (dir? ch) dpre fpre) %]))))
        sort-dir (cond
                   *dirs-first* (dir-sorter "0" "1")
                   *dirs-last* (dir-sorter "1" "0")
                   :else identity)]
    (sort-by (comp first filter-ci sort-dir) 
             (if *lex-sort* compare natural-compare) 
             chs)))

(defn split-lone-ch
  "Splits a root-only tree if it is not a directory."
  [[par & chs :as tree]]
  (let [[head tail] (split-last par)]
    (if (and (empty? chs) (seq head) (seq tail)) [head [tail]] tree)))

(defn tree->lines
  [tree]
  (letfn [(do-childs
           [result pfx [ch & chs]]
           (let [pfx-curr (conj pfx (if (empty? chs) lst tee))
                 pfx-desc (conj pfx (if (empty? chs) spa bra))]
             (if ch
               (do-childs (do-branch result pfx-curr pfx-desc ch) pfx chs)
               result)))
          (do-branch
           [result pfx-par pfx-chs [par & chs]]
           (->> chs
                (map split-lone-ch)
                (sort-chs)
                (do-childs (conj result (conj pfx-par par)) pfx-chs)))]
    (when (first tree) (do-branch [] [] [] tree))))

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
                         (sort)
                         (reduce add-path [])
                         (tree->lines))]
            (println (str/join x))))))))

;; :project pptree
;;    :author Mike Weaver
;;    :created 2020-05-23
;;    
;; :section Introduction
;; 
;;    Takes a list of paths and produces a printout similar to *nix `tree`
;;    command.

(ns wevre.pptree
  (:require [clojure.string :as str]))

(def ^:dynamic *sep* "/")

(defn split-last 
  "Split a path at final sep."
  [s]
  (if-let [i (str/last-index-of s *sep*)]
    (map str/join (split-at (inc i) s))
    (list "" s)))

(defn get-prefix
  "Return common prefix ending with sep."
  [a b & xs]
  (->> (apply map vector a b xs)
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
                #_(sort-chs)
                (do-childs (conj result (conj pfx-par par)) pfx-chs))
           #_(let [chs (sort-chs (map split-lone-ch chs))]
               (do-childs (conj result (conj pfx-par par)) pfx-chs chs)))]
    (when (first tree) (do-branch [] [] [] tree))))

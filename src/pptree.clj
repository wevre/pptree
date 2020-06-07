;; :project pptree
;;    :author Mike Weaver
;;    :created 2020-05-23
;;    
;; :section Introduction
;; 
;;    Takes a list of paths and returns a printout similar to *nix `tree`
;;    command.

(ns pptree
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
  (let [chain
        (loop [[par & _ :as tree] tree pars [] path path]
          (let [pfx (get-prefix par path) de-path (deprefix path pfx)]
            (cond
              (empty? tree) (cons [path] pars)
              (pos? (compare par pfx)) (cons [pfx (deprefix tree pfx) [de-path]] pars)
              (= path pfx) (cons tree pars)   ;; Ignore duplicate directories.
              (or (= 1 (count tree)) (empty? (get-prefix (first (peek tree)) de-path)))
              (cons (conj tree [de-path]) pars)   ;; Add path as (newest) last child.
              :else   ;; Add path to (existing) last child.
              (recur (peek tree) (cons (pop tree) pars) de-path))))]
    (reduce #(conj %2 %1) chain)))
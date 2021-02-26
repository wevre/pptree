(ns wevre.pptree.tree
  "Methods for converting paths into nodes, and organizing them into trees."
  (:require [clojure.string :as str]
            [clojure.zip :as z]))

(def ^:dynamic *sep* "/")

(defn dir? [path] (str/ends-with? path *sep*))

(defn node<-path
  "Returns a node (map) from a path (string)."
  [path]
  (let [[root & ch] (if-let [i (str/last-index-of path *sep*)]
                      [(subs path 0 (inc i)) (subs path (inc i))]
                      [path ""])
        childs (seq (map #(hash-map :root %) (remove #{""} ch)))]
    (cond-> {:root root}
      childs (assoc :childs childs))))

(defn common-prefix
  "Return shared prefix between two nodes."
  [a b]
  (let [pfx (->> (map vector (:root a) (:root b))
                 (take-while (partial apply =))
                 (map first)
                 str/join)]
    (if (dir? pfx) pfx "")))

(defn zipper<-tree
  [tree]
  (z/zipper
   (fn branch? [{root :root}] (dir? root))
   :childs
   (fn make-node [node childs] (assoc node :childs childs))
   tree))

(comment   ; Testing zippers...
  (let [loc (zipper<-tree (node<-path "/h/a/"))]
    (z/down loc))   ;=> nil, no children
  (let [loc (zipper<-tree (node<-path "/h/a/"))]
    (z/root (z/insert-child loc (node<-path "b"))))
  (let [loc (zipper<-tree (node<-path "h"))]
    (z/branch? loc))   ;=> false
  ;
  )

(defn de-prefix [node pfx] 
  (let [node (update node :root str/replace-first pfx "")]
    (if (and (= "" (:root node)) (= 1 (count (:childs node))))
      (first (:childs node))
      node)))

(defn re-root
  [loc node pfx]
  (z/replace loc {:root pfx :childs (->> (list (z/node loc) node)
                                         (map #(de-prefix % pfx))
                                         (sort-by :root))}))
(defn add-node
  [tree node]
  (z/root
   ((fn [loc node]
      (let [tree (z/node loc), pfx (common-prefix tree node)]
        (if (or (seq pfx) (not (z/up loc)))
          (cond   ; common prefix means tree is a branch
            (neg? (compare pfx (:root tree))) (re-root loc node pfx)
            (= node tree) loc
            (z/down loc) (recur (z/down loc) (de-prefix node pfx))
            :else (z/insert-child loc (de-prefix node pfx)))
          (cond   ; no common prefix and not at top of tree
            (neg? (compare (:root node) (:root tree))) (z/insert-left loc node)
            #_#_(= node tree) loc   ; suppress duplicates?
            (z/right loc) (recur (z/right loc) node)
            :else (z/insert-right loc node)))))
    (zipper<-tree tree) node)))

(comment
  (let [input ["/h/w/a" "/h/w/b"]]
    (de-prefix (->> input (map node<-path) (reduce add-node)) "/h/w/")))

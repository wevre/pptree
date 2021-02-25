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

(comment
  (let [inp ["" "h" "h/" "h/a" "h/a/" "/" "/h" "/h/" "/h/a" "/h/a/"]]
    (map node<-path inp))
  ;
  )

(defn common-prefix
  "Return shared prefix between two nodes."
  [a b]
  (let [pfx (->> (map vector (:root a) (:root b))
                 (take-while (partial apply =))
                 (map first)
                 str/join)]
    (if (dir? pfx) pfx "")))

(comment
  (let [input [["" "/h"] ["/h/aa" "/h/ab"] ["h/w" "h/g"] ["b" "b"]]]
    (map (fn [[a b]] (common-prefix (node<-path a) (node<-path b))) input))
  (let [input [["" "|h"] ["|h|a" "|h|b"] ["|h|w|ca" "|h|w|cb"] ["h|w" "h|g"]]]
    (binding [*sep* "|"]
      (map (fn [[a b]] (common-prefix (node<-path a) (node<-path b))) input)))
  ;
  )

(defn zipper<-node
  [tree]
  (z/zipper
   (fn branch? [{root :root}] (dir? root))
   :childs
   (fn make-node [node childs] (assoc node :childs childs))
   tree))

(comment
  (let [loc (zipper<-node (node<-path "/h/a/"))]
    (z/down loc))   ;=> nil, no children
  (let [loc (zipper<-node (node<-path "/h/a/"))]
    (z/insert-child loc (node<-path "b")))
  (let [loc (zipper<-node (node<-path "h"))]
    (z/branch? loc))
  ;
  )

(defn de-prefix [node pfx] 
  (let [node (update node :root str/replace-first pfx "")]
    (if (and (= "" (:root node))
             (= 1 (count (:childs node))))
      (first (:childs node))
      node)))

(comment
  (let [node (node<-path "/h/w/a")] (de-prefix node "/h/"))
  (let [node (node<-path "/h/w/a")] (de-prefix node "/h/w/"))
  ;
  )

(defn re-root
  [loc node pfx]
  (z/replace loc {:root pfx :childs (->> (list (z/node loc) node)
                                         (map #(de-prefix % pfx))
                                         (sort-by :root))}))

(comment
  (let [tree (node<-path "/h/w/a")
        loc (zipper<-node tree)
        node (node<-path "/h/b")
        pfx (common-prefix tree node)]
    (z/root (re-root loc node pfx)))
  ;
  )

(defn add-node
  [tree node]
  (z/root
   ((fn [loc node]
      (let [tree (z/node loc), pfx (common-prefix tree node)]
        (if (or (seq pfx) (not (z/up loc)))
          (cond   ; common prefix means tree is a branch
            (neg? (compare pfx (:root tree))) (re-root loc node pfx)
            (z/down loc) (recur (z/down loc) (de-prefix node pfx))
            :else (z/insert-child loc (de-prefix node pfx)))
          (cond   ; no common prefix and not at top of tree
            (neg? (compare (:root node) (:root tree))) (z/insert-left loc node)
            #_#_(= node tree) loc   ; suppress duplicates?
            (z/right loc) (recur (z/right loc) node)
            :else (z/insert-right loc node)))))
    (zipper<-node tree) node)))

(comment
  (let [input ["/h/w/a" "/h/w/b"]]
    (->> input (map node<-path) (reduce add-node)))
  (let [input ["/h/w" "l"]]
    (->> input (map node<-path) (reduce add-node)))
  (let [input ["/h/b/c" "/h/a"]]
    (->> input (map node<-path) (reduce add-node)))
  (let [input ["/h/b/c" "/h/d"]]
    (->> input (map node<-path) (reduce add-node)))
  (let [input ["/h/b/" "/h/b/"]]
    (->> input (map node<-path) (reduce add-node)))
  (let [input ["/h/" "/h/b"]]
    (->> input (map node<-path) (reduce add-node)))
  (let [input ["/h/a" "/h/b/c" "/h/b/d"]]
    (->> input (map node<-path) (reduce add-node)))
  (let [input ["/h/b" "/h/d" "/h/a"]]
    (->> input (map node<-path) (reduce add-node)))
  (let [input ["/h/b" "/h/d" "/h/c"]]
    (->> input (map node<-path) (reduce add-node)))
  (let [input ["/h/b" "/h/d" "/h/e"]]
    (->> input (map node<-path) (reduce add-node)))
  ;
  )

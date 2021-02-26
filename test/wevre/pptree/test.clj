(ns wevre.pptree.test
  (:require [wevre.pptree.tree :as t]
            [wevre.pptree.print :as p]
            [wevre.pptree.main :as m]
            [clojure.string :as str]
            [clojure.zip :as z]
            [clojure.test :refer [deftest are is testing]]))

(defn tree<-input 
  "Similar to same-named method in wevre.pptree.main, but doesn't sort childs."
  [input]
  (->> input (map t/node<-path) (reduce t/add-node)))

(deftest pptree
  (testing "node from path"
    (are [path node] (= node (t/node<-path path))
      ""                   {:root ""}
      "hello"              {:root "hello"}
      "hello/"             {:root "hello/"}
      "hello/world"        {:root "hello/" :childs '({:root "world"})}
      "hello/world/"       {:root "hello/world/"}
      "hello/world/alfa"   {:root "hello/world/" :childs '({:root "alfa"})}
      "/"                  {:root "/"}
      "/hello"             {:root "/" :childs '({:root "hello"})}
      "/hello/"            {:root "/hello/"}
      "/hello/world"       {:root "/hello/" :childs '({:root "world"})}
      "/hello/world/"      {:root "/hello/world/"}
      "/hello/world/bravo" {:root "/hello/world/" :childs '({:root "bravo"})}))

  (testing "common prefix"
    (are [a b pfx] (= pfx (t/common-prefix (t/node<-path a) (t/node<-path b)))
      ""                     "/hello"               ""
      ""                     "/world"               ""
      ""                     ""                     ""
      "hello"                "world"                ""
      "/hello/world/alfa"    "/hello/world/bravo"   "/hello/world/"
      "/hello/world/charlie" "/hello/world/chicago" "/hello/world/"
      "/hello/world/delta"   "/hello/test/echo"     "/hello/"
      "hello/world/foxtrot"  "frank/george/henry"   ""))

  (testing "de-prefix"
    (is (= '{:root "world/" :childs ({:root "alfa"})}
           (t/de-prefix (t/node<-path "/hello/world/alfa") "/hello/"))
        "-- remove first part of prefix")

    (is (= '{:root "alfa"}
           (t/de-prefix (t/node<-path "/hello/world/alfa") "/hello/world/"))
        "-- remove entire prefix, collapse")
    
    (is (= '{:root "" :childs ({:root "alfa"} {:root "bravo"})}
           (t/de-prefix (tree<-input ["/hello/alfa" "/hello/bravo"]) "/hello/"))
        "-- remove entire prefix but with multiple children"))
  
  (testing "re-root"
    (is (= '{:root "/hello/"
             :childs ({:root "bravo"} {:root "world/"
                                       :childs ({:root "alfa"})})}
           (let [tree (t/node<-path "/hello/world/alfa")
                 node (t/node<-path "/hello/bravo")
                 pfx (t/common-prefix tree node)]
             (z/root (t/re-root (t/zipper<-tree tree) node pfx))))
        "-- re-root"))

  (testing "add path"
    (is (= '{:root "/hello/world/" :childs ({:root "alpha"} {:root "bravo"})}
           (tree<-input ["/hello/world/alpha" "/hello/world/bravo"]))
        "-- re-root under shared parent")

    (is (= '{:root "" :childs ({:root "/hello/" :childs ({:root "world"})}
                                {:root "alpha"})}
           (tree<-input ["/hello/world" "alpha"]))
        "-- re-root with empty prefix")

    (is (= '{:root "/hello/"
             :childs ({:root "alfa"}
                      {:root "bravo/" :childs ({:root "charlie"})})}
           (tree<-input ["/hello/bravo/charlie" "/hello/alfa"]))
        "-- re-root before")

    (is (= '{:root "/hello/"
             :childs ({:root "bravo/" :childs ({:root "charlie"})}
                      {:root "delta"})}
           (tree<-input ["/hello/bravo/charlie" "/hello/delta"]))
        "-- re-root after")

    (is (= '{:root "/hello/charlie/"}
           (tree<-input ["/hello/charlie/" "/hello/charlie/"]))
        "-- path == prefix (duplicate)")

    (is (= '{:root "/hello/" :childs ({:root "alpha"})}
           (tree<-input ["/hello/" "/hello/alpha"]))
        "-- first-ever child")

    (is (= '{:root "/hello/"
             :childs ({:root "alpha"} {:root "bravo/"
                                       :childs ({:root "charlie"}
                                                {:root "delta"})})}
           (tree<-input ["/hello/alpha" "/hello/bravo/charlie" "/hello/bravo/delta"]))
        "-- insert into existing child")

    (is (= '{:root "/hello/" 
             :childs ({:root "alpha"} {:root "bravo"} {:root "delta"})}
           (tree<-input ["/hello/bravo" "/hello/delta" "/hello/alpha"]))
        "-- add as first child")

    (is (= '{:root "/hello/"
             :childs ({:root "bravo"} {:root "charlie"} {:root "delta"})}
           (tree<-input ["/hello/bravo" "/hello/delta" "/hello/charlie"]))
        "-- add as middle child")

    (is (= '{:root "/hello/"
             :childs ({:root "bravo"} {:root "delta"} {:root "echo"})}
           (tree<-input ["/hello/bravo" "/hello/delta" "/hello/echo"]))
        "-- add as last child"))

  (testing "lines from tree"
    (is
     (= ["/h/"
         "├── a"
         "├── a/"
         "│   ├── b"
         "│   └── b/"
         "│       └── c"
         "└── d"]
        (->> (tree<-input ["/h/a" "/h/a/b" "/h/a/b/c" "/h/d"]) p/lines<-tree))))

  (testing "test options"
    (let [input ["/hello/alpha/echo"
                 "/hello/alpha/charlie/file10.txt"
                 "/hello/alpha/Delta"
                 "/hello/alpha/charlie/file1.txt"
                 "/hello/alpha/bravo"
                 "/hello/alpha/charlie/file2.txt"]
          get-lines (fn [inp] (doall (p/lines<-tree (m/tree<-input inp))))]
      (is (= ["|hello|alpha|"
              "├── bravo"
              "├── charlie|"
              "│   ├── file1.txt"
              "│   ├── file2.txt"
              "│   └── file10.txt"
              "├── Delta"
              "└── echo"]
             (binding [t/*sep* "|"] (->> input
                                         (map #(str/replace % "/" "|"))
                                         get-lines)))
          "-- different sep")

      (is (= ["/hello/alpha/"
              "├── bravo"
              "├── charlie/"
              "│   ├── file1.txt"
              "│   ├── file2.txt"
              "│   └── file10.txt"
              "├── Delta"
              "└── echo"]
             (get-lines input))
          "-- normal sorting")

      (is (= ["/hello/alpha/"
              "├── charlie/"
              "│   ├── file1.txt"
              "│   ├── file2.txt"
              "│   └── file10.txt"
              "├── bravo"
              "├── Delta"
              "└── echo"]
             (binding [m/*dirs-first* true] (get-lines input)))
          "-- dir first")

      (is (= ["/hello/alpha/"
              "├── bravo"
              "├── Delta"
              "├── echo"
              "└── charlie/"
              "    ├── file1.txt"
              "    ├── file2.txt"
              "    └── file10.txt"]
             (binding [m/*dirs-last* true] (get-lines input)))
          "dir last")

      (is (= ["/hello/alpha/"
              "├── Delta"
              "├── bravo"
              "├── charlie/"
              "│   ├── file1.txt"
              "│   ├── file2.txt"
              "│   └── file10.txt"
              "└── echo"]
             (binding [m/*case-sens* true] (get-lines input)))
          "-- case sensitive")

      (is (= ["/hello/alpha/"
              "├── bravo"
              "├── charlie/"
              "│   ├── file1.txt"
              "│   ├── file10.txt"
              "│   └── file2.txt"
              "├── Delta"
              "└── echo"]
             (binding [m/*lex-sort* true] (get-lines input)))
          "-- lexical sort"))))

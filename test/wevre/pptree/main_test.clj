(ns wevre.pptree.main-test
  (:require [wevre.pptree.main :as sut]
            [clojure.test :refer [deftest are is testing]]
            [clojure.string :as str]))

(deftest pptree
  (testing "split last"
    (are [path head tail] (= (list head tail) (sut/split-last path))
      ""                   ""              ""
      "hello"              ""              "hello"
      "hello/"             "hello/"        ""
      "hello/world"        "hello/"        "world"
      "hello/world/"       "hello/world/"  ""
      "hello/world/alfa"   "hello/world/"  "alfa"
      "/"                  "/"             ""
      "/hello"             "/"             "hello"
      "/hello/"            "/hello/"       ""
      "/hello/world"       "/hello/"       "world"
      "/hello/world/"      "/hello/world/" ""
      "/hello/world/bravo" "/hello/world/" "bravo"))

  (testing "common prefix"
    (are [a b pfx] (= pfx (sut/common-prefix a b))
      ""                     "/hello"               ""
      nil                    "/world"               ""
      nil                    nil                    ""
      "/hello/world/alfa"    "/hello/world/bravo"   "/hello/world/"
      "/hello/world/charlie" "/hello/world/chicago" "/hello/world/"
      "/hello/world/delta"   "/hello/test/echo"     "/hello/"
      "hello/world/foxtrot"  "frank/george/henry"   ""))

  (testing "add-path"
    (is (= ["/hello"]
           (sut/add-path [] "/hello"))
        "-- empty tree")
    (is (= ["/hello/" ["alfa"] ["bravo"]]
           (sut/add-path ["/hello/alfa"] "/hello/bravo"))
        "-- prefix shorter than parent")
    (is (= ["" ["hello"] ["world"]]
           (sut/add-path ["hello"] "world"))
        "-- no common prefix")
    (is (= ["/hello/charlie/"]
           (sut/add-path ["/hello/charlie/"] "/hello/charlie/"))
        "-- path == prefix (duplicate)")
    (is (= ["/hello/world/" ["delta"]]
           (sut/add-path ["/hello/world/"] "/hello/world/delta"))
        "-- childless tree")
    (is (= ["/hello/" ["alfa"] ["bravo"] ["charlie"]]
           (sut/add-path ["/hello/" ["alfa"] ["bravo"]] "/hello/charlie"))
        "-- no common prefix with last child, add as next child")
    (is (= ["/hello/" ["alfa"] ["bravo/" ["charlie"] ["delta"]]]
           (sut/add-path ["/hello/" ["alfa"] ["bravo/charlie"]]
                         "/hello/bravo/delta"))
        "-- add to and replace last child"))

  (testing "lines from tree"
    (is
     (= [["/h/"]
         ["├── " "l/"]
         ["│   " "├── " "a"]
         ["│   " "├── " "b"]
         ["│   " "└── " "c"]
         ["└── " "w/"]
         ["    " "├── " "p"]
         ["    " "├── " "q"]
         ["    " "└── " "q/"]
         ["    " "    " "└── " "r"]]
        (sut/lines<-tree
         ["/h/" ["l/" ["a"] ["b"] ["c"]] ["w/" ["p"] ["q"] ["q/r"]]]))))
  
  (testing "sort DIR first"
    (let [input ["/hello/alpha/charlie/file2.txt"
                 "/hello/alpha/charlie/file10.txt"
                 "/hello/alpha/Epsilon"
                 "/hello/alpha/delta"
                 "/hello/alpha/bravo"
                 "/hello/alpha/charlie/file1.txt"]
          get-lines (fn [inp] (->> inp
                                   sort
                                   (reduce sut/add-path [])
                                   sut/lines<-tree
                                   (map str/join)))]
      (is (= (get-lines input)
             ["/hello/alpha/"
              "├── bravo"
              "├── charlie/"
              "│   ├── file1.txt"
              "│   ├── file2.txt"
              "│   └── file10.txt"
              "├── delta"
              "└── Epsilon"])
          "normal sorting")
      (is (= (binding [sut/*dirs-first* true] (get-lines input))
             ["/hello/alpha/"
              "├── charlie/"
              "│   ├── file1.txt"
              "│   ├── file2.txt"
              "│   └── file10.txt"
              "├── bravo"
              "├── delta"
              "└── Epsilon"])
          "dir first")
      (is (= (binding [sut/*dirs-last* true] (get-lines input))
             ["/hello/alpha/"
              "├── bravo"
              "├── delta"
              "├── Epsilon"
              "└── charlie/"
              "    ├── file1.txt"
              "    ├── file2.txt"
              "    └── file10.txt"])
          "dir last")
      (is (= (binding [sut/*case-sens* true] (get-lines input))
             ["/hello/alpha/"
              "├── Epsilon"
              "├── bravo"
              "├── charlie/"
              "│   ├── file1.txt"
              "│   ├── file2.txt"
              "│   └── file10.txt"
              "└── delta"])
          "case sensitive")
      (is (= (binding [sut/*lex-sort* true] (get-lines input))
             ["/hello/alpha/"
              "├── bravo"
              "├── charlie/"
              "│   ├── file1.txt"
              "│   ├── file10.txt"
              "│   └── file2.txt"
              "├── delta"
              "└── Epsilon"])
          "lexical sort"))))

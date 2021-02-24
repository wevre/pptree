(ns wevre.pptree.main-test
  (:require [wevre.pptree.main :as sut]
            [clojure.test :refer [deftest are is testing]]))

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
  
  (testing "add path"
    (is (= ["/hello/alpha"]
           (sut/add-path [] "/hello/alpha"))
        "-- empty tree")
    (is (= ["/hello/" "alfa" "bravo"]
           (sut/add-path ["/hello/alfa"] "/hello/bravo"))
        "-- re-root under shared parent")
    (is (= ["/hello/" "alfa" ["bravo/" "charlie"]]
           (sut/add-path ["/hello/bravo/" "charlie"] "/hello/alfa"))
        "-- re-root before")
    (is (= ["/hello/" ["alfa/" "charlie"] "delta"]
           (sut/add-path ["/hello/alfa/" "charlie"] "/hello/delta"))
        "-- re-root after")
    (is (= ["" "hello" "world"]
           (sut/add-path ["hello"] "world"))
        "-- re-root with empty prefix")
    (is (= ["/hello/charlie/"]
           (sut/add-path ["/hello/charlie/"] "/hello/charlie/"))
        "-- path == prefix (duplicate)")
    (is (= ["/hello/" "alpha"]
           (sut/add-path ["/hello/"] "/hello/alpha"))
        "-- first-ever child")
    (is (= ["/hello/" "alpha" ["bravo/" "charlie" "delta"]]
            (sut/add-path ["/hello/" "alpha" ["bravo/" "charlie"]]
                           "/hello/bravo/delta"))
        "-- insert into existing child")
    (is (= ["/hello/" "alfa" "bravo" "charlie"]
           (sut/add-path ["/hello/" "alfa" "bravo" "charlie"]
                          "/hello/bravo"))
        "-- don't add duplicate child")
    (is (= ["/hello/" "alfa" "bravo" "charlie"]
           (sut/add-path ["/hello/" "bravo" "charlie"] "/hello/alfa"))
        "-- add as first child")
    (is (= ["/hello/" "alfa" "bravo" "charlie"]
           (sut/add-path ["/hello/" "alfa" "charlie"] "/hello/bravo"))
        "-- add as middle child")
    (is (= ["/hello/" "alfa" "bravo" "charlie"]
           (sut/add-path ["/hello/" "alfa" "bravo"] "/hello/charlie"))
        "-- add as last child"))
  
  (testing "lines from tree"
    (is
     (= ["/h/"
         "├── l/"
         "│   ├── a"
         "│   ├── b"
         "│   └── c"
         "└── w/"
         "    ├── p"
         "    ├── q"
         "    └── q/"
         "        └── r"]
        (sut/lines<-tree
         '("/h/" ("l/" "a" "b" "c") ("w/" "p" "q" ("q/" "r")))))))
  
  (testing "test options"
    (let [input ["/hello/alpha/charlie/file2.txt"
                 "/hello/alpha/charlie/file10.txt"
                 "/hello/alpha/Epsilon"
                 "/hello/alpha/delta"
                 "/hello/alpha/bravo"
                 "/hello/alpha/charlie/file1.txt"]
          get-lines (fn [inp] (sut/lines<-tree (sut/tree<-input inp)))]
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

(ns wevre.pptree.main-test
  (:require [wevre.pptree.main :as sut]
            [clojure.test :refer [deftest are is testing]]))

(deftest test-split-last
  (are [path head tail]
       (= (list head tail) (sut/split-last path))
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

(deftest test-get-prefix
  (are [a b pfx]
       (= pfx (sut/get-prefix a b))
    ""                     "/hello"               ""
    nil                    "/world"               ""
    nil                    nil                    ""
    "/hello/world/alfa"    "/hello/world/bravo"   "/hello/world/"
    "/hello/world/charlie" "/hello/world/chicago" "/hello/world/"
    "/hello/world/delta"   "/hello/test/echo"     "/hello/"
    "hello/world/foxtrot"  "frank/george/henry"   ""))

(deftest test-add-path
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
        "-- add to and replace last child")))

(deftest test-add-path*
  (testing "add-path*"
    (is (= ["/hello"]
           (sut/add-path* [] "/hello"))
        "-- empty tree")
    (is (= ["/hello/" ["alfa"] ["bravo"]]
           (sut/add-path* ["/hello/alfa"] "/hello/bravo"))
        "-- prefix shorter than parent")
    (is (= ["" ["hello"] ["world"]]
           (sut/add-path* ["hello"] "world"))
        "-- no common prefix")
    (is (= ["/hello/charlie/"]
           (sut/add-path* ["/hello/charlie/"] "/hello/charlie/"))
        "-- path == prefix (duplicate)")
    (is (= ["/hello/world/" ["delta"]]
           (sut/add-path* ["/hello/world/"] "/hello/world/delta"))
        "-- childless tree")
    (is (= ["/hello/" ["alfa"] ["bravo"] ["charlie"]]
           (sut/add-path* ["/hello/" ["alfa"] ["bravo"]] "/hello/charlie"))
        "-- no common prefix with last child, add as next child")
    (is (= ["/hello/" ["alfa"] ["bravo/" ["charlie"] ["delta"]]]
           (sut/add-path* ["/hello/" ["alfa"] ["bravo/charlie"]]
                             "/hello/bravo/delta"))
        "-- add to and replace last child")))

(deftest test-tree->lines
  (testing "tree->lines"
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
        (sut/tree->lines
         ["/h/" ["l/" ["a"] ["b"] ["c"]] ["w/" ["p"] ["q"] ["q/r"]]])))))

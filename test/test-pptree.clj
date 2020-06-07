(ns test-pptree
  (:require [pptree]
            [clojure.test :refer [deftest are is testing]]))

(deftest test-split-last
  (are [path head tail]
       (= (list head tail) (pptree/split-last path))
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
       (= pfx (pptree/get-prefix a b))
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
           (pptree/add-path [] "/hello"))
        "-- empty tree")
    (is (= ["/hello/" ["alfa"] ["bravo"]]
           (pptree/add-path ["/hello/alfa"] "/hello/bravo"))
        "-- prefix shorter than parent")
    (is (= ["" ["hello"] ["world"]]
           (pptree/add-path ["hello"] "world"))
        "-- no common prefix")
    (is (= ["/hello/charlie/"]
           (pptree/add-path ["/hello/charlie/"] "/hello/charlie/"))
        "-- path == prefix (duplicate)")
    (is (= ["/hello/world/" ["delta"]]
           (pptree/add-path ["/hello/world/"] "/hello/world/delta"))
        "-- childless tree")
    (is (= ["/hello/" ["alfa"] ["bravo"] ["charlie"]]
           (pptree/add-path ["/hello/" ["alfa"] ["bravo"]] "/hello/charlie"))
        "-- no common prefix with last child, add as next child")
    (is (= ["/hello/" ["alfa"] ["bravo/" ["charlie"] ["delta"]]]
           (pptree/add-path ["/hello/" ["alfa"] ["bravo/charlie"]]
                             "/hello/bravo/delta"))
        "-- add to and replace last child")))

(deftest test-add-path*
  (testing "add-path*"
    (is (= ["/hello"]
           (pptree/add-path* [] "/hello"))
        "-- empty tree")
    (is (= ["/hello/" ["alfa"] ["bravo"]]
           (pptree/add-path* ["/hello/alfa"] "/hello/bravo"))
        "-- prefix shorter than parent")
    (is (= ["" ["hello"] ["world"]]
           (pptree/add-path* ["hello"] "world"))
        "-- no common prefix")
    (is (= ["/hello/charlie/"]
           (pptree/add-path* ["/hello/charlie/"] "/hello/charlie/"))
        "-- path == prefix (duplicate)")
    (is (= ["/hello/world/" ["delta"]]
           (pptree/add-path* ["/hello/world/"] "/hello/world/delta"))
        "-- childless tree")
    (is (= ["/hello/" ["alfa"] ["bravo"] ["charlie"]]
           (pptree/add-path* ["/hello/" ["alfa"] ["bravo"]] "/hello/charlie"))
        "-- no common prefix with last child, add as next child")
    (is (= ["/hello/" ["alfa"] ["bravo/" ["charlie"] ["delta"]]]
           (pptree/add-path* ["/hello/" ["alfa"] ["bravo/charlie"]]
                             "/hello/bravo/delta"))
        "-- add to and replace last child")))

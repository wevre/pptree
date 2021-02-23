(ns wevre.pptree.cli
  ^{:see-also [["https://github.com/clojure/tools.cli"
                "Clojure cli tools"]]}
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]))

(def cli-options
  [["-I" "--case-sensitive" "Do case sensitive sort."]
   ["-F" "--folders-first" "Sort folders first."]
   ["-f" "--folders-last" "Sort folders last."]
   ["-l" "--lexical-sort" "Turn off natural sorting and use lexical sorting."]
   ["-h" "--help"]])

(defn usage [opts-summ]
  (->> ["pptree -- Prints arbitrary strings in a tree structure."
        ""
        "Usage: pptree [options] [sep]"
        ""
        "options:"
        opts-summ
        ""
        "sep:"
        "  string to use as path separator. Use quotes if it might be confused"
        "  with shell commands, such as `|`, `>`, etc."
        ""]
       (str/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) {:exit-message (usage summary) :ok? true}
      errors {:exit-message (error-msg errors)}
      (and (:folders-first options) (:folders-last options))
      {:exit-message (error-msg ["Specify only one of -f or -F."])}
      :else
      {:separator (first arguments) :options options})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

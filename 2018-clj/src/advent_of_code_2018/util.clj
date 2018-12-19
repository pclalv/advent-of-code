(ns advent-of-code-2018.util)

(defn abs [n] (max n (- n)))

(defn input-file-contents [input-file]
  (->> input-file
       (str "resources/")
       (slurp)))

(defn input-file-lines [input-file]
  (-> input-file
      (input-file-contents)
      (clojure.string/split #"\n")))

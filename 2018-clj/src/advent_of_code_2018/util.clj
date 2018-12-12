(ns advent-of-code-2018.util)

(comment (defn input-file-lines [input-file]
           (-> (str "resources/day_" day "/" input-file)
               (slurp)
               (clojure.string/split #"\n"))))

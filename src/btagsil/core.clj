(ns btagsil.core
  (:require [btagsil.actions :refer [repl-execute]]))

(defn repl-once []
  (let [prompt ">> "]
    (print prompt)
    (flush)
    (read-line)))

(defn repl-run []
  (loop [input (repl-once)]
    (repl-execute input)
    (recur (repl-once))))

(defn -main [& _args]
  (repl-run))

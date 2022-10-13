(ns btagsil.core
  (:require [btagsil.actions :refer [repl-execute]]
            [btagsil.world :refer [init-world]]))

(defn repl-once []
  (let [prompt ">> "]
    (print prompt)
    (flush)
    (read-line)))

(defn repl-run []
  (loop [world init-world
         input (repl-once)]
    (recur (repl-execute input world) (repl-once))))

(defn -main [& _args]
  (repl-run))

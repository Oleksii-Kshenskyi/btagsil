(ns btagsil.core
  (:require [clojure.string :refer [lower-case]]
            [btagsil.actions :refer [repl-execute]]
            [btagsil.world :refer [init-world]]))

(defn repl-once []
  (let [prompt ">> "]
    (print prompt)
    (flush)
    (lower-case (read-line))))

(defn repl-run []
  (loop [world init-world
         _wia (repl-execute "where am i" world)
         input (repl-once)]
    (recur (repl-execute input world) _wia (repl-once))))

(defn -main [& _args]
  (repl-run))

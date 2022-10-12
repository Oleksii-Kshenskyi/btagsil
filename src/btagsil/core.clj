(ns btagsil.core)

(defn repl-once []
  (let [prompt ">> "]
    (print prompt)
    (flush)
    (read-line)))

(defn repl-run []
  (loop [input (repl-once)]
    (println (str input "\n"))
    (recur (repl-once))))

(defn -main [& _args]
  (repl-run))

(ns btagsil.actions
  (:require [clojure.core.match :refer [match]]
            [clojure.string :refer [join split]]))

(defn get-action [tags]
  (match (first tags)
    "exit"  [:exit]
    "echo"  [:respond (join " " (rest tags))]
    nil     [:empty]
    :else   [:unknown (join " " tags)]))

(defn repl-exit []
  (println "Thanks for playing! Come again!\n")
  (System/exit 0))
(defn repl-respond [what]
  (println (str what "\n")))
(defn repl-unknown [what]
  (println (str "Umm... What is '" what "'?\n")))
(defn repl-empty [])
(defn repl-execute [input]
  (match (get-action (filter not-empty (split input #"\s")))
    [:exit] (repl-exit)
    [:respond what] (repl-respond what)
    [:empty] (repl-empty)
    [:unknown what] (repl-unknown what)))
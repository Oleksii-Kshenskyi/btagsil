(ns btagsil.actions
  (:require [clojure.core.match :refer [match]]
            [clojure.string :refer [join split]]))

(defn get-action [tags]
  (match (first tags)
    "exit"  [:exit]
    "echo"  [:respond (join " " (rest tags))]
    nil     [:empty]
    :else   [:unknown (join " " tags)]))

(defn repl-exit [world]
  (println "Thanks for playing! Come again!\n")
  (System/exit 0)
  world)
(defn repl-respond [what world]
  (println (str what "\n"))
  world)
(defn repl-unknown [what world]
  (println (str "Umm... What is '" what "'?\n"))
  world)
(defn repl-empty [world] world)
(defn repl-execute [input world]
  (match (get-action (filter not-empty (split input #"\s")))
    [:exit] (repl-exit world)
    [:respond what] (repl-respond what world)
    [:empty] (repl-empty world)
    [:unknown what] (repl-unknown what world)))
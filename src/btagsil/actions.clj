(ns btagsil.actions
  (:require [clojure.core.match :refer [match]]
            [clojure.string :refer [join split]]
            [btagsil.world :as w]))

(defn where-loc-error [args] (str "Sorry, I can only tell you where you are, not '"
                                  (join " " args)
                                  "'!\nTry using 'where am i'."))
(defn act-where [tags world]
  (match (into [] tags)
    ["where" "am" "i"] (w/current-loc-description world)
    ["where"] (str "Where what?")
    ["where" "can" "i" "go"] (w/possible-destinations world)
    ["where" "is" thing] (str "I don't know where '" thing "' is ¯\\_(ツ)_/¯")
    [_where & args] (where-loc-error args)))

(defn get-action [tags world]
  (match (first tags)
    "exit"  [:exit]
    "where" [:respond (act-where tags world)]
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
  (match (get-action (filter not-empty (split input #"\s")) world)
    [:exit] (repl-exit world)
    [:respond what] (repl-respond what world)
    [:empty] (repl-empty world)
    [:unknown what] (repl-unknown what world)))
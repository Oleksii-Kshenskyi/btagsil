(ns btagsil.actions
  (:require [clojure.core.match :refer [match]]
            [clojure.string :refer [join split]]
            [btagsil.world :as world]
            [btagsil.data :as data]))

(defn act-where [tags world]
  (match (vec tags)
    ["where" "am" "i"] (world/current-loc-description world)
    ["where"] (data/where-error)
    ["where" "can" "i" "go"] (world/possible-destinations world)
    ["where" "is" & thing] (data/where-is-error (join " " thing))
    [_where & args] (data/where-loc-error (join " " args))))

(defn act-look [tags world]
  (match (vec tags)
    ["look" "at" & what] (world/look-at world what)
    :else (data/look-error (rest tags))))

(defn get-action [tags world]
  (match (first tags)
    "exit"  [:exit]
    "where" [:respond (act-where tags world)]
    "look"  [:respond (act-look tags world)]
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
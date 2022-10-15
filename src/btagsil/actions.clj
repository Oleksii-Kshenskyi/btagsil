(ns btagsil.actions
  (:require [clojure.core.match :refer [match]]
            [clojure.string :refer [join split]]
            [btagsil.world :as world]
            [btagsil.data :as data]))

;; Respond actions:

(defn act-where [world tags]
  (match (vec tags)
    ["where" "am" "i"] (world/current-loc-description world)
    ["where"] (data/where-error)
    ["where" "can" "i" "go"] (world/possible-destinations world)
    ["where" "is"] (data/where-is-what-error)
    ["where" "is" & thing] (data/where-is-error (join " " thing))
    [_where & args] (data/where-loc-error (join " " args))))

(defn act-look [world tags]
  (match (vec tags)
    ["look" "at" & what] (world/look-at world what)
    :else (data/look-error (rest tags))))

;; Change actions:

(defn respond-go [changed-world tags]
  (match (vec tags)
    ["go" "to"] (data/go-to-where-error)
    ["go" "to" & where] (world/went-to changed-world where)
    :else (data/go-error (rest tags))))

(defn change-go [world tags]
  (match (vec tags)
    ["go" "to" & where] (world/set-loc world where)
    :else world))

(defn perform-go [world tags]
  (let [changed-world (change-go world tags)
        response (respond-go changed-world tags)]
    [changed-world response]))

;; Here we decide which action to perform based on the first word of the user input.

(defn get-action [world tags]
  (match (first tags)
    "exit"  [:exit]
    "where" [:respond (act-where world tags)]
    "look"  [:respond (act-look world tags)]
    "echo"  [:respond (join " " (rest tags))] 
    "go"    [:change (perform-go world tags)]
    nil     [:empty]
    :else   [:unknown (join " " tags)]))

;; Types of REPL's reactions to user input:

(defn repl-exit [world]
  (println "Thanks for playing! Come again!\n")
  (System/exit 0)
  world)

(defn repl-respond [world what]
  (println (str what "\n"))
  world)

(defn repl-unknown [world what]
  (println (str "Umm... What is '" what "'?\n"))
  world)

(defn repl-empty [world] world)

;; Main func for the execution of a single REPL action

(defn repl-execute [world input]
  (match (get-action world (filter not-empty (split input #"\s")))
    [:exit] (repl-exit world)
    [:respond what] (repl-respond world what)
    [:change [changed-world response]] (repl-respond changed-world response)
    [:empty] (repl-empty world)
    [:unknown what] (repl-unknown world what)))
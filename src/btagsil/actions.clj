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
    ["look" "around"] (world/what-is-here world)
    :else (data/look-error (rest tags))))

(defn act-show [world tags]
  (match (vec tags)
    ["show" "world"] (world/show world)
    :else (data/show-error)))

(defn act-what [world tags]
  (match (vec tags)
    ["what"] (data/what-error)
    ["what" "is"] (data/what-is-what-error)
    ["what" "is" & what] (world/what-is world what)
    ["what" & _nope] (data/what-error)))

;; Change actions:

(defn respond-talk [world tags]
  (match (vec tags)
    ["talk" "to"] (data/talk-to-what-error)
    ["talk"] (data/talk-error)
    ["talk" "to" & object-name-vec] (world/talk-to-object world object-name-vec)
    ["talk" & _nope] (data/talk-error)))

(defn change-talk [world _tags] world)

(defn perform-talk [world tags]
  (let [changed-world (change-talk world tags)
        response (respond-talk world tags)]
    [changed-world response]))


(defn respond-go [changed-world tags from]
  (match (vec tags)
    ["go" "to"] (data/go-to-where-error)
    ["go" "to" & where] (world/went-to changed-world where from)
    :else (data/go-error (rest tags))))

(defn change-go [world tags from]
  (match (vec tags)
    ["go" "to"] world
    ["go" "to" & where] (world/set-loc world where from)
    :else world))

(defn perform-go [world tags]
  (let [from (world/current-loc-id world)
        changed-world (change-go world tags from)
        response (respond-go changed-world tags from)]
    [changed-world response]))

;; Here we decide which action to perform based on the first word of the user input.

(defn get-action [world tags]
  (match (first tags)
    "exit"  [:exit]
    "where" [:respond (act-where world tags)]
    "what"  [:respond (act-what world tags)]
    "look"  [:respond (act-look world tags)]
    "echo"  [:respond (join " " (rest tags))]
    "show"  [:debug (act-show world tags)]
    "go"    [:change (perform-go world tags)]
    "talk"  [:change (perform-talk world tags)]
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

(defn repl-debug [world what]
  (println (str "[DEBUG INFO] " what "\n"))
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
    [:debug what] (repl-debug world what)
    [:change [changed-world response]] (repl-respond changed-world response)
    [:empty] (repl-empty world)
    [:unknown what] (repl-unknown world what)))
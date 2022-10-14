(ns btagsil.data)

;;; Weapons data

(defn init-weapon [name description]
  {:name name
   :description description})

(defn init-fists [] (init-weapon "bare fists"
                                 "just your bare fists"))
(defn init-axe [] (init-weapon "a greataxe"
                               "an enormous razor-sharp greataxe with blades on both ends of the hilt."))
(defn init-sword [] (init-weapon "a claymore"
                                 "a long beautiful claymore with an ornamental pattern on the blade."))
(defn init-bow [] (init-weapon "a greatbow"
                               "a terrifying greatbow with spear-sized arrows."))

;;; Player data

(defn init-player []
  {:current-location :forest
   :weapon (init-fists)})

;;; Location data

(defn init-location [id name short-name description connected]
  {id {:name name
       :short-name short-name
       :description description
       :connected connected}})

(defn init-forest []
  (init-location :forest
                 "a beautiful rainforest"
                 "a rainforest"
                 (str "overflowing with gorgeous trees and grass.\n" 
                      "The fresh smell of nature after rain is in the air.")
                 [:square]))
(defn init-square []
  (init-location :square
                 "a busy square full of people"
                 "a square"
                 "so grand you're starting to feel a little nauseous."
                 [:forest]))

;;; Text helpers

;;; Location helpers

(defn describe-loc [name description]
  (str "You're in " name ".\nIt's " description))

(defn you-can-go-to [short-name-chain]
  (str "You can go to " short-name-chain " from here."))

(defn you-went-to [where]
  (str "You went to " where "."))

;;; Weapon helpers

(defn look-at-weapon [description]
  (str "You see " description "."))

;;; Error helpers

(defn go-error [_tags]
  (str "I can only go 'to' places."))

(defn go-to-where-error []
  (str "Go to... where?"))

(defn no-such-loc-error [where-str]
  (str "Whoops, " where-str " is not a place you can go to!"))

(defn look-error [_what]
  (str "I can only look 'at' stuff."))

(defn look-at-error [what]
  (str "I don't know how to look at "
       what
       ".\nMaybe you meant 'look at weapon'?"))

(defn look-at-what-error [] (str "Look at what?"))

(defn where-error [] (str "Where what?"))

(defn where-loc-error [what] (str "Sorry, I can only tell you where you are, not '"
                                  what
                                  "'!\nTry using 'where am i'."))

(defn where-is-error [thing]
  (str "I don't know where '" thing "' is ¯\\_(ツ)_/¯"))
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

;;; Location objects

(defn init-object [name description properties]
  {:name name
   :description description
   :properties properties})

(defn init-guard []
  (init-object "a guard"
               (str "a big muscular man in heavy armor wielding a halberd.\n"
                    "He looks friendly and loves talking to strangers visiting the square")
                    [:talks]))

(defn guard-pandoras-box []
  [
   "How's your day going today, Your Highness?"
   "Did you know that dragons can fly?"
   "I've been thinking about the purpose of my life a lot recently... Why am I here?"
   "I want to kiss a girl so much..."
   "I'm a bit under the weather today, apologies."
   "You look lovely today, Your Highness!"
   "WOW! The hat REALLY suits you! So stylish!"
   "It's important to remember to brush your teeth every morning."
   "A bottle of fine ale would hit the spot right about now..."
   "It's impressive how quickly these tourists litter the square. Ugh."
   "Did you know there's a fine weapon shop just nearby? Try going there!"
   "Are you tired?"
   "I remember that time I was a wee little lad..."
  ])

(defn init-weapon-shop-shopkeeper []
  (init-object "a shopkeeper"
               (str "a shopkeeper running the weapon shop.\n"
                    "He's throwing glances at you hoping you'll buy a weapon from him")
               [:talks, :sells]))

;;; Location data

(defn init-location [name short-name description connected objects]
  {:name name
   :short-name short-name
   :description description
   :connected connected
   :objects objects})

(defn init-forest []
  (init-location "a beautiful rainforest"
                 "a forest"
                 (str "overflowing with gorgeous trees and grass.\n" 
                      "The fresh smell of nature after rain is in the air.")
                 [:square]
                 []))

;; TODO: There should be a guard on the square. He says random stuff when you talk to him.
(defn init-square []
  (init-location "a busy square full of people"
                 "a square"
                 "so grand you're starting to feel a little nauseous."
                 [:forest, :weapon-shop]
                 {:guard (init-guard)}))

;; TODO: There should be a shopkeeper in the shop. He can sell you weapons.
(defn init-weapon-shop []
  (init-location "a weapon shop"
                 "a weapon shop"
                 (str "chock-full with all sorts of weapons to buy.\n"
                      "The shopkeeper is watching you closely, both curious and wary.")
                 [:square]
                 {:shopkeeper (init-weapon-shop-shopkeeper)}))

;; TODO: There should be a cave. You can fight a monster inside.

;;; Text helpers

;;; Location helpers

(defn describe-loc [name description]
  (str "You're in " name ".\nIt's " description))

(defn you-can-go-to [short-name-chain]
  (str "You can go to " short-name-chain " from here."))

(defn you-went-to [where]
  (str "You went to " where "."))

;; Location object helpers

(defn guard-says [guard-says guard-name]
  (str guard-name " says: '" guard-says "'"))

(defn talk-to-shopkeeper [_world shopkeeper-name]
  (str shopkeeper-name " says: '"
       "Stop talking and buy something already, you flirtatious vagabond!"
       "'"))

(defn you-see [what]
  (str "You see " what " here."))

(defn look-at-object [descr]
  (str "You see " descr "."))

;;; Weapon helpers

(defn look-at-weapon [description]
  (str "You see " description "."))

;;; Error helpers

(defn no-object-to-talk-to-error [object-name-str]
  (str "You don't seem to see any " object-name-str "s that would be willing to talk."))

(defn does-not-talk-error [object-name-str]
  (str "The " object-name-str " doesn't seem to react."))

(defn talk-error [] (str "I can only talk 'to' stuff."))

(defn talk-to-what-error [] (str "Talk to... what?"))

(defn go-error [_tags]
  (str "I can only go 'to' places."))

(defn go-to-where-error []
  (str "Go to... where?"))

(defn no-such-loc-error [where-str]
  (str "Whoops, " where-str " is not a place you can go to!"))

(defn already-there-error [where-str]
  (str "You cannot go to '" where-str, "', you're already there!"))

(defn cant-get-there-from-here-error [current-loc where-str]
  (str "You cannot go to '" where-str "' from '" current-loc "'.\n"
       "'where can i go' can help you learn where you can go from '" current-loc "'!"))

(defn look-error [_what]
  (str "I can only look 'at' stuff."))

(defn look-at-error [what]
  (str "You don't see any "
       what
       "s around.\n"
       "You can say 'look at my weapon' to look at your weapon.\n"
       "You can also look at things that are around here.\n"
       "Say 'what is here' to see what you can look at."))

(defn look-at-what-error [] (str "Look at what?"))

(defn show-error [] (str "NOPE: I can only show 'world' for now."))
(defn show-world [world] (str "Current world is: " world))

(defn where-error [] (str "Where what?"))

(defn where-loc-error [what] (str "Sorry, I can only tell you where you are, not '"
                                  what
                                  "'!\nTry using 'where am i'."))

(defn where-is-error [thing]
  (str "I don't know where '" thing "' is ¯\\_(ツ)_/¯"))

(defn where-is-what-error []
  (str "Where is what?"))

(defn what-error []
  (str "Sorry, I can only answer questions like 'what is <something>'."))

(defn what-is-what-error []
  (str "What is... what?"))

(defn you-see-nothing []
  (str "You don't see anything important here."))

(defn dont-know-what-is [object]
  (str "Sorry, I don't know what '" object "' is."))
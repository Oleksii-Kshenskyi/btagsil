(ns btagsil.data)

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


(defn describe-loc [name description]
  (str "You're in " name ".\nIt's " description))

(defn you-can-go-to [short-name-chain]
  (str "You can go to " short-name-chain " from here."))
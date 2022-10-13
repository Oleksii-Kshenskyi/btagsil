(ns btagsil.world)

(require '[clojure.string :refer [join]])

(defn init-location [id name short-name description connected]
  {id {:name name :short-name short-name :description description :connected connected}})

(defn apply-nce [n f]
  (apply comp (repeat n f)))
(defn decompress [m]
  (apply hash-map ((apply-nce 3 #(apply concat %)) m)))

(defn get-location [world loc]
  (get-in world [:locations loc]))
(defn current-loc-id [world]
  (get-in world [:player :current-location]))

(defn get-current-loc [world]
  (let [loc-id (current-loc-id world)
        curr-loc (get-location world loc-id)]
    curr-loc))

(defn current-loc-description [world]
  (let [curr-loc (get-current-loc world)
        name (get-in curr-loc [:name])
        descr (get-in curr-loc [:description])]
  (str "You're in " name ".\nIt's " descr)))
(defn short-name-by-key [world key] (get-in (get-location world key) [:short-name]))
(defn you-can-go-here [world connected]
  (map #(short-name-by-key world %) connected))
(defn possible-destinations [world]
  (let [curr-loc (get-current-loc world)
        connected (get-in curr-loc [:connected])]
    (str "You can go to " (join ", " (you-can-go-here world connected)) " from here.")))

(def init-world
  {:player {:current-location :forest}
   :locations (decompress {(init-location :forest
                              "a beautiful rainforest"
                              "a rainforest"
                              (str "overflowing with gorgeous trees and grass.\n"
                                   "The fresh smell of nature after rain is in the air.")
                              [:square])
               (init-location :square
                              "a busy square full of people"
                              "a square"
                              "so grand you're starting to feel a little nauseous."
                              [:forest])})})
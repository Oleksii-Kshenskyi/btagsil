(ns btagsil.world)

(defn init-location [id name description connected]
  {id {:name name :description description :connected connected}})

(defn apply-nce [n f]
  (apply comp (repeat n f)))
(defn decompress [m]
  (apply hash-map ((apply-nce 3 #(apply concat %)) m)))

(defn get-location [world loc]
  (get-in world [:locations loc]))

(def init-world
  {:player {:current-location :forest}
   :locations (decompress {(init-location :forest
                              "a beautiful rainforest"
                              (str "overflowing with gorgeous trees and grass.\n"
                                   "The pure smell of nature after rain is in the air.")
                              [:square])
               (init-location :square
                              "a busy square full of people"
                              "so grand you're starting to feel a little nauseous."
                              [:forest])})})
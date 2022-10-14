(ns btagsil.world
  (:require [clojure.string :refer [join]]
            [btagsil.data :as data]))

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
        name (:name curr-loc)
        descr (:description curr-loc)]
  (data/describe-loc name descr)))

(defn short-name-by-key [world key]
  (->> (get-location world key)
       (:short-name)))

(defn connected-short-names [world connected]
  (->> connected
       (map #(short-name-by-key world %))
       (join ", ")))

(defn possible-destinations [world]
  (let [curr-loc (get-current-loc world)
        connected (:connected curr-loc)
        names (connected-short-names world connected)]
  (data/you-can-go-to names)))

(def init-world
  {:player {:current-location :forest}
   :locations (decompress {(data/init-forest)
                           (data/init-square)})})
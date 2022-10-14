(ns btagsil.world
  (:require [clojure.string :refer [join]]
            [btagsil.data :as data]
            [clojure.core.match :refer [match]]))

(defn apply-nce [n f]
  (apply comp (repeat n f)))
(defn decompress [m]
  (apply hash-map ((apply-nce 3 #(apply concat %)) m)))

(defn get-location [world loc]
  (get-in world [:locations loc]))
(defn current-loc-id [world]
  (get-in world [:player :current-location]))
(defn location-keyword [world loc]
  (let [keyword (keyword loc)]
    (if (.contains (vec (keys (:locations world))) keyword)
      keyword
      nil)))
(defn short-name-by-key [world key]
  (->> (get-location world key)
       (:short-name)))

(defn get-current-loc [world]
  (let [loc-id (current-loc-id world)
        curr-loc (get-location world loc-id)]
    curr-loc))
(defn set-current-loc [world loc]
  (assoc-in world [:player :current-location] loc))

(defn current-loc-description [world]
  (let [curr-loc (get-current-loc world)
        name (:name curr-loc)
        descr (:description curr-loc)]
  (data/describe-loc name descr)))

(defn go-response-by-keyword [world loc-keyword]
  (data/you-went-to (short-name-by-key world loc-keyword)))
(defn set-loc [world where]
  (let [where-str (join " " where)
        loc-keyword (location-keyword world where-str)]
    (when loc-keyword
      (set-current-loc world loc-keyword))))
(defn went-to [world where]
  (let [where-str (join " " where)
        loc-keyword (location-keyword world where-str)]
    (if loc-keyword
      (go-response-by-keyword world loc-keyword)
      (data/no-such-loc-error where-str))))

(defn connected-short-names [world connected]
  (->> connected
       (map #(short-name-by-key world %))
       (join ", ")))

(defn possible-destinations [world]
  (let [curr-loc (get-current-loc world)
        connected (:connected curr-loc)
        names (connected-short-names world connected)]
  (data/you-can-go-to names)))

(defn current-weapon-description [world]
  (get-in world [:player :weapon :description]))

(defn look-at [world what]
  (let [descr (current-weapon-description world)
        what-str (join " " what)]
    (match what
      []         (data/look-at-what-error)
      ["weapon"] (data/look-at-weapon descr)
      :else      (data/look-at-error what-str))))

(def init-world
  {:player (data/init-player)
   :locations (decompress {(data/init-forest) ; TODO: new location: weapon shop
                           (data/init-square)})})
(ns btagsil.world
  (:require [clojure.string :as str]
            [btagsil.data :as data]
            [clojure.core.match :refer [match]]))

;; Helpers

(defn str->keyword [orig-str]
  (keyword (str/replace orig-str #" " "-")))

;; Working with locations

(defn get-location [world loc]
  (get-in world [:locations loc]))

(defn current-loc-id [world]
  (get-in world [:player :current-location]))

(defn location-keyword [world loc]
  (let [keyword (str->keyword loc)]
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

(defn can-go [world from to]
  (.contains (:connected (get-location world from)) to))

(defn set-current-loc [world loc]
  (assoc-in world [:player :current-location] loc))

(defn current-loc-description [world]
  (let [curr-loc (get-current-loc world)
        name (:name curr-loc)
        descr (:description curr-loc)]
  (data/describe-loc name descr)))

;; Working with objects

(defn get-object-by-keyword [loc keyword]
  (get-in loc [:objects keyword]))

;; The go action (first implemented action that changes the world)

(defn go-response-by-keyword [world loc-keyword]
  (data/you-went-to (short-name-by-key world loc-keyword)))

(defn validate-route [world to-keyword from]
  (let [already-there (= from to-keyword)
        no-such-loc (= to-keyword nil)
        valid-route (can-go world from to-keyword)]
    (match [already-there, no-such-loc, valid-route]
      [_, true, _] :no-such-loc
      [true, _, _] :already-there
      [_, _, false] :invalid-route
      :else :valid)))

(defn set-loc [world where from]
  (let [where-str (str/join " " where)
        loc-keyword (location-keyword world where-str)
        validated (validate-route world loc-keyword from)]
    (match validated
      :valid (set-current-loc world loc-keyword)
      :else world)))

(defn went-to [world where from]
  (let [where-str (str/join " " where)
        loc-keyword (location-keyword world where-str)
        validated (validate-route world loc-keyword from)]
    (match validated
      :valid (go-response-by-keyword world loc-keyword)
      :already-there (data/already-there-error where-str)
      :no-such-loc (data/no-such-loc-error where-str)
      :invalid-route (data/cant-get-there-from-here-error from where-str))))

;; Navigational actions

(defn connected-short-names [world connected]
  (->> connected
       (map #(short-name-by-key world %))
       (str/join ", ")))

(defn possible-destinations [world]
  (let [curr-loc (get-current-loc world)
        connected (:connected curr-loc)
        names (connected-short-names world connected)]
  (data/you-can-go-to names)))

(defn current-weapon-description [world]
  (get-in world [:player :weapon :description]))

(defn look-at-object [world what]
  (let [object-keyword (str->keyword what)
        current-loc (get-current-loc world)
        object (get-object-by-keyword current-loc object-keyword)]
    (match object
      nil (data/look-at-error what)
      o (data/look-at-object (:description o)))))

(defn look-at [world what]
  (let [descr (current-weapon-description world)
        what-str (str/join " " what)]
    (match what
      []         (data/look-at-what-error)
      ["my" "weapon"] (data/look-at-weapon descr)
      :else      (look-at-object world what-str))))

(defn what-is-here [world]
  (let [objects (:objects (get-current-loc world))
        object-bodies (map #(nth % 1) objects)
        descrs (map :name object-bodies)]
    (match (vec descrs)
      [] (data/you-see-nothing)
      x (data/you-see (str/join ", " x)))))

(defn what-is [world what]
  (let [object (str/join " " what)]
    (match (vec what)
      ["here"] (what-is-here world)
      :else (data/dont-know-what-is object))))

(defn show [world]
  (data/show-world world))

;; The world initialization func

(def init-world
  {:player (data/init-player)
   :locations {:forest (data/init-forest) ; TODO: new location: cave
               :square (data/init-square)
               :weapon-shop (data/init-weapon-shop)}})
(ns tetris-android.android.core
  (:require [reagent.core :as r :refer [atom]]
            [re-frame.core :refer [subscribe dispatch dispatch-sync]]
            [tetris-android.events]
            [tetris-android.subs]
            [clojure.string :as str]
            [clojure.set :as sets]))

(def ReactNative (js/require "react-native"))
(def ReactSvg (js/require "react-native-svg"))

(def app-registry (.-AppRegistry ReactNative))
(def view (r/adapt-react-class (.-View ReactNative)))
(def touchable-highlight (r/adapt-react-class (.-TouchableHighlight ReactNative)))
(def text (r/adapt-react-class (.-Text ReactNative)))


(def svg (r/adapt-react-class (.-Svg ReactSvg)))
(def rect (r/adapt-react-class (.-Rect ReactSvg)))

(def height 500)
(def width 400)

(def colours ["red" "lime" "yellow" "aqua" "fuchsia" "blue"])
(def piece-type [:rectangle :square :zig-zag-left :zig-zag-right :t-shape :l-shape])

(defonce game (atom []))
(defonce status (atom ""))
(defonce game-updater (atom 0))

(defn overlaps
  [{first-blocks :blocks} {second-blocks :blocks}]
  (let [contains-block (fn [blocks {x :x y :y}] (not= 0 (count (filter #(and (== x (:x %1)) (== y (:y %1))) blocks))))]
    (some #(contains-block second-blocks %1) first-blocks)))

(defn is-valid-world
  [state new-piece]
  (let [at-bottom-edge (some #(< height (+ 25 (:y %1))) (:blocks new-piece))
        at-left-edge (some #(< (:x %1) 0) (:blocks new-piece))
        at-right-edge (some #(> (+ 25 (:x %1)) width) (:blocks new-piece))
        pieces-overlap (not= 0 (count (filter #(overlaps new-piece %1) state)))]
    (not (or at-bottom-edge at-left-edge at-right-edge pieces-overlap))))

(defn apply-direction
  [piece direction]
  (let [blocks (:blocks piece)]
    (cond
     (= direction :down) (assoc piece :blocks (map #(assoc %1 :y (+ (:y %1) 25)) blocks))
     (= direction :left) (assoc piece :blocks (map #(assoc %1 :x (- (:x %1) 25)) blocks))
     (= direction :right) (assoc piece :blocks (map #(assoc %1 :x (+ (:x %1) 25)) blocks))
     :else piece)))

(defn row-complete?
  [state]
  (let [xs (set (range 0 width 25))
        blocks (flatten (map :blocks state))]
    (not-empty (filterv (fn [[k v]] (empty? (sets/difference xs (set (map :x v))))) (group-by :y blocks)))))

(defn clear-complete-row
  [state]
  (println state)
  (let [xs (set (range 0 width 25))
        blocks (flatten (map :blocks state))
        ys-to-remove (set (keys (filter (fn [[k v]] (empty? (sets/difference xs (set (map :x v))))) (group-by :y blocks))))
        updated (mapv (fn [piece] (->> (:blocks piece)
                                       (filter #(not (contains? ys-to-remove (:y %1))))
                                       (map #(assoc %1 :y (+ (:y %1) (* 25 (count (filter (fn [y] (< (:y %1) y)) ys-to-remove))))))
                                       (assoc piece :blocks))) state)]
    (println ys-to-remove updated)
    updated))

(defn update-piece
  [piece]
  )

(defn move-piece
  [state direction]
  (let [piece (last state)
        current (vec (take (- (count state) 1) state))
        current-state (if (row-complete? current) (clear-complete-row current) current)
        new-piece (apply-direction piece direction)
        new-state (conj current-state new-piece)]
    (if (is-valid-world current-state new-piece)
      new-state
      (conj current-state piece))))

(def rotations-by-piece
  {:rectangle
   [[{:x 25 :y -25} {:x 0 :y 0} {:x -25 :y 25} {:x -50 :y -50}]
    [{:x -25 :y 25} {:x 0 :y 0} {:x 25 :y -25} {:x 50 :y 50}]]
   :zig-zag-left
   [[{:x 0 :y -25} {:x 25 :y 0} {:x 0 :y 25} {:x 25 :y 50}]
    [{:x 0 :y 25} {:x -25 :y 0} {:x 0 :y -25} {:x -25 :y -50}]]
   :zig-zag-right
   [[{:x 50 :y 25} {:x 25 :y 0} {:x 0 :y 25} {:x -25 :y 0}]
    [{:x -50 :y -25} {:x -25 :y 0} {:x 0 :y -25} {:x 25 :y 0}]]
   :t-shape
   [[{:x 25 :y -25} {:x 0 :y 0} {:x -25 :y 25} {:x -25 :y -25}]
    [{:x 25 :y 25} {:x 0 :y 0} {:x -25 :y -25} {:x 25 :y -25}]
    [{:x -25 :y 25} {:x 0 :y 0} {:x 25 :y -25} {:x 25 :y 25}]
    [{:x -25 :y -25} {:x 0 :y 0} {:x 25 :y 25} {:x -25 :y 25}]]
   :l-shape
   [[{:x 50 :y 25} {:x 25 :y 0} {:x 0 :y -25} {:x -25 :y 0}]
    [{:x -25 :y 25} {:x 0 :y 0} {:x 25 :y -25} {:x 0 :y -50}]
    [{:x -25 :y -25} {:x 0 :y 0} {:x 25 :y 25} {:x 50 :y 0}]
    [{:x 0 :y -50} {:x -25 :y -25} {:x -50 :y 0} {:x -25 :y 25}]]
   })

(defn apply-rotation
  [piece]
  (let [default [[{:x 0 :y 0} {:x 0 :y 0} {:x 0 :y 0} {:x 0 :y 0}]]
        rotation-index (get piece :rotated 0)
        rotation (get rotations-by-piece (:type piece) default)
        current-rotation (nth rotation rotation-index)
        new-rotation-index (rem (inc rotation-index) (count rotation))
        rotated-blocks (map #(apply merge-with + %1) (partition 2 (interleave (:blocks piece) current-rotation)))]
    (assoc piece :rotated new-rotation-index :blocks rotated-blocks)))

(defn rotate-piece
  [state]
  (let [piece (last state)
        current-state (vec (take (- (count state) 1) state))
        new-piece (apply-rotation piece)
        new-state (conj current-state new-piece)]
    (if (is-valid-world current-state new-piece)
      new-state
      (conj current-state piece))))

(defn select-random
  [data]
  (data (rand-int (count data))))

(defn make-piece
  [id x colour piece-type]
  (cond
   (= piece-type :rectangle) {:type :rectangle :colour colour :blocks [{:id id :x x :y 0} {:id (+ 1 id) :x (+ 25 x) :y 0} {:id (+ 2 id) :x (+ 50 x) :y 0} {:id (+ 3 id) :x (+ 75 x) :y 0}]}
   (= piece-type :square) {:type :square :colour colour :blocks [{:id id :x x :y 0} {:id (+ 1 id) :x (+ 25 x) :y 0} {:id (+ 2 id) :x x :y 25} {:id (+ 3 id) :x (+ 25 x) :y 25}]}
   (= piece-type :zig-zag-left) {:type :zig-zag-left :colour colour :blocks [{:id id :x x :y 50} {:id (+ 1 id) :x x :y 25} {:id (+ 2 id) :x (+ 25 x) :y 25} {:id (+ 3 id) :x (+ 25 x) :y 0}]}
   (= piece-type :zig-zag-right) {:type :zig-zag-right :colour colour :blocks [{:id id :x x :y 0} {:id (+ 1 id) :x x :y 25} {:id (+ 2 id) :x (+ 25 x) :y 25} {:id (+ 3 id) :x (+ 25 x) :y 50}]}
   (= piece-type :t-shape) {:type :t-shape :colour colour :blocks [{:id id :x x :y 0} {:id (+ 1 id) :x (+ 25 x) :y 0} {:id (+ 2 id) :x (+ 50 x) :y 0} {:id (+ 3 id) :x (+ 25 x) :y 25}]}
   (= piece-type :l-shape) {:type :l-shape :colour colour :blocks [{:id id :x x :y 0} {:id (+ 1 id) :x x :y 25} {:id (+ 2 id) :x x :y 50} {:id (+ 3 id) :x (+ 25 x) :y 50}]}
   :else {}))

(defn add-piece
  [state]
  (let [random-colour (select-random colours)
        random-piece-type (select-random piece-type)
        possible-x (vec (range 0 (- width 100) 25))
        random-x (select-random possible-x)
        new-piece (make-piece (* 4 (count state)) random-x random-colour random-piece-type)]
    (if (is-valid-world state new-piece)
      (conj state new-piece)
      (do
        (js/clearInterval @game-updater)
        (reset! status "Game Over")
        state))))

(defn update-game
  [state]
  (let [new-state (move-piece state :down)]
    (if (= state new-state)
      (js/setTimeout #(swap! game add-piece) 50))
    new-state))

(def movement-keys {37 :left 39 :right 40 :down})
(defn handle-keyboard-input
  [event]
  (let [key (.-keyCode event)]
    (cond
     (contains? movement-keys key) (swap! game move-piece (movement-keys key))
     (= 38 key) (swap! game rotate-piece))))

(defn gravity
  []
  (reset! game-updater (js/setInterval #(swap! game update-game) 500)))

(defonce start (gravity))
(defonce adder (swap! game add-piece))

(defn restart
  []
  (do
    (js/clearInterval @game-updater)
    (reset! game [])
    (gravity)))

(defn rectangle
  [{id :id x :x y :y} colour]
  [rect {:width 25 :height 25 :x x :y y :key id :fill colour :stroke "black" :stroke-width 1}])

(defn tetris []
  [svg {:width width :height height}
   (for [piece @game]
     (for [block (:blocks piece)]
       (rectangle block (:colour piece))))])

(defn app-root []
  (let [greeting (subscribe [:get-greeting])]
    (fn []
      [view {:style {:flex-direction "column" :margin 40 :align-items "center"}}
       (tetris)
       [view {:style {:flex 1 :flex-direction "row" :margin 10}}
        [touchable-highlight {:style {:background-color "#999" :padding 25 :border-radius 5}
                             :on-press #(restart)}
         [text {:style {:color "white" :text-align "center" :font-weight "bold"}} "restart"]]]])))

(defn init []
      (dispatch-sync [:initialize-db])
      (.registerComponent app-registry "TetrisAndroid" #(r/reactify-component app-root)))

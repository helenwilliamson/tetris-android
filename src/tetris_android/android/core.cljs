(ns tetris-android.android.core
  (:require [reagent.core :as r :refer [atom]]
            [re-frame.core :refer [subscribe dispatch dispatch-sync]]
            [tetris-android.events]
            [tetris-android.subs]))

(def ReactNative (js/require "react-native"))
(def ReactSvg (js/require "react-native-svg"))

(def app-registry (.-AppRegistry ReactNative))
(def view (r/adapt-react-class (.-View ReactNative)))
(def svg (r/adapt-react-class (.-Svg ReactSvg)))
(def rect (r/adapt-react-class (.-Rect ReactSvg)))



(defn alert [title]
      (.alert (.-Alert ReactNative) title))

(defn app-root []
  (let [greeting (subscribe [:get-greeting])]
    (fn []
      [view {:style {:flex-direction "column" :margin 40 :align-items "center"}}
       [svg {:height "500" :width "400"}
        [rect {:x 340 :y 440 :width 50 :height 50}]]])))

(defn init []
      (dispatch-sync [:initialize-db])
      (.registerComponent app-registry "TetrisAndroid" #(r/reactify-component app-root)))

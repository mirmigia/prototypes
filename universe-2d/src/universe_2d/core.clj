(ns universe-2d.core
  (:require [clojure.math.combinatorics :as comb]
            [loom.attr :as lattr]
            [loom.alg :as lalg]
            [loom.graph :as loom]
            [loom.gen :as lgen]
            [loom.io :as lio]
            [quil.core :as q])
  (:import (java.util.concurrent ThreadLocalRandom))
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn map-key-value
  "Same as `map`, but works on hash-maps.
  `fk` is applied to each key in and `fv` to each value."
  [fk fv m]
  (into {} (for [[k v] m] [(fk k) (fv v)])))

(defn map-keys
  "Same as `map`, but works the keys of hash-maps."
  [f m]
  (map-key-value f identity m))

(defn map-values
  "Same as `map`, but works on the values of hash-maps."
  [f m]
  (map-key-value identity f m))

;; TODO: better name
(defn graph-reduce
  "Reduce over graph `g` using function `f` with initial value as `g`."
  [g f]
  (reduce f g (loom/nodes g)))

(defn graph-length
  "Returns the length (number of nodes) in graph `g`."
  [g]
  (count (loom/nodes g)))

(defn add-attrs-to-graph-nodes
  "Add the map of attributes `attrs` in to every node defined
  in `nodes` for a graph. Defaults to using all nodes."
  ([g attrs] (add-attrs-to-graph-nodes g attrs (loom/nodes g)))
  ([g attrs nodes]
   (reduce (fn [acc [k v]]
             (lattr/add-attr-to-nodes acc k v nodes))
           g attrs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn square [x] (* x x))

(defn rand-int
  "Returns a random integer between `min` (inclusive) and `max` (exclusive).
  `min` is 0 by default."
  ([max] (rand-int 0 max))
  ([min max]
   (-> (ThreadLocalRandom/current)
       (.nextInt min max))))

(defn rand-pos
  "Takes a rectangle boundary and returns a random X and Y position
  within it."
  [[x y w h]]
  [(rand-int x w) (rand-int y h)])

;; TODO: optimize
;; TODO: write docs
(defn foldv [f]
  (fn [& [v1 & vecs]]
    (reduce (fn [[acc-x acc-y] [x y]]
              [(f acc-x x) (f acc-y y)])
            v1 vecs)))

(def v+ "Add one or more vectors." (foldv +))
(def v- "Subtract one or more vectors." (foldv -))
(def v* "Multiply one or more vectors." (foldv *))
;; / doesn't work, as its used for namespacing
(def v% "Divide one or more vectors." (foldv /))

(defn vmag-squared
  "Squared magnitude of vector."
  [[x y]]
  (+ (square x) (square y)))

(defn vmag
  "Magnitude of vector `v`. Prefer using vmag-squared over this as square
  rooting is an expensive operation."
  [v]
  (Math/sqrt (vmag-squared v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Universe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private area 500000)
(def ^:private velocity [1/2 1/2])
#_(def ^:private gravity [10 10])

;; FIXME: Normally we want to start at the center of the universe and
;; FIXME: simulate a sort of "big bang". The fruchterman & reingold algorithm
;; FIXME: AFAIK requires nodes to be in a random position, so having position
;; FIXME: set to 0 is not going to work. - Antonis (2016-12-30)
#_(defn gen-universe
  "Generate a universe with `n` number of solar systems."
  [n & {:keys [init-pos] :or {init-pos [0.0 0.0]}}]
  (let [g (lgen/gen-rand-p (loom/digraph) n (rand 0.25))]
    (add-attrs-to-graph-nodes g {:position init-pos})))

(defn gen-universe
  "Generate a universe with `n` number of solar systems. Places each
  solar system randomly between `bounds`"
  [n & {:keys [bounds]}]
  (let [g (lgen/gen-rand-p (loom/digraph) n (rand 0.25))]
    (graph-reduce g #(lattr/add-attr %1 %2 :position (rand-pos bounds)))))

;; TODO: better name
(defn apply-force [force-fn [x1 y1] [x2 y2]]
  (let [xdist (- x1 x2)
        ydist (- y1 y2)
        dist (Math/sqrt (+ (square xdist) (square ydist)))]
    (if (> dist 0)
      (let [force (force-fn dist)]
        [(* (/ xdist dist) force)
         (* (/ ydist dist) force)])
      [0.0 0.0])))

(defn- node-repulsion
  "Returns the repulsive force (as a vector) of two vectors,
  using force constant `k`."
  [k v1 v2]
  (apply-force #(/ (square k) %) v1 v2))

(defn- node-attraction
  "Returns the attractive force (as a vector) of two vectors,
  using force constant `k`."
  [k v1 v2]
  (apply-force #(/ (square %) k) v1 v2))

(defn- node-displacement [g f k edges]
  (->> edges
       (map (fn [e]
              [e (->> (map #(lattr/attr g % :position) e)
                      (apply f k))]))
       (into {})))

(defn- repulsion-displacement
  "Takes a graph `g` and force constant `k` and returns the repulsive diplacement
  between 2 nodes represented as `{[n1 n2] [x y]}`"
  [g k]
  (->> (comb/selections (loom/nodes g) 2)
       (remove #(apply = %)) ;; remove duplicate nodes E.g. (7, 7)
       (map vec)             ;; ensure that we have a vector
       (node-displacement g node-repulsion k)))

(defn- attraction-displacement
  "Takes a graph `g` and force constant `k` and returns the attractive diplacement
  between 2 nodes represented as `{[n1 n2] [x y]}`"
  [g k]
  (node-displacement g node-attraction k (loom/edges g)))

(defn- sum-displacements [disp]
  (reduce (fn [acc [e disp]]
            (if (get acc e)
              (update acc (first e) v+ disp)
              (assoc acc (first e) disp)))
          {} disp))

(defn limit-displacement
  "Limits the displacement for node to remain within area"
  [area [xdist ydist]]
  (let [dist (Math/sqrt (+ (square xdist) (square ydist)))
        max-disp (/ (Math/sqrt area) 10.0)] ;; FIXME: Magic number 10.0
    (if (> dist 0)
      ;; FIXME: use velocity magnitude
      (let [limited-dist (min (* max-disp (vmag velocity)) dist)]
        [(* (/ xdist dist) limited-dist)
         (* (/ ydist dist) limited-dist)])
      [0.0 0.0])))

(defn move-nodes
  "Returns an updated graph `g` with the new node positions according
  to the displacements `disps`."
  [g disps]
  (let [positions (->> (loom/nodes g)
                       (map (fn [n]
                              [n (lattr/attr g n :position)]))
                       (into {}))]
    (graph-reduce g (fn [g n]
                      (let [disp (get disps n)
                            pos  (get positions n)]
                        (->> (v* disp velocity)
                             (v+ pos)
                             (lattr/add-attr g n :position)))))))

(defn step-universe
  "Advance universe simulation. Does not modify `universe`, it returns
  a new version."
  [universe]
  (let [k (/ (Math/sqrt area) (inc (graph-length universe)))
        repl (repulsion-displacement universe k)
        attr (attraction-displacement universe k)]
    (->> (map sum-displacements [repl attr])
         (apply merge-with v-)
         (map-values #(limit-displacement area %))
         ;; Apply displacement to position
         (move-nodes universe))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: pick dynamically according to size of screen & number of nodes
(def system-diameter 25)

(def universe (atom nil))

(defn draw! []
  (let [universe (swap! universe step-universe)]
    (q/clear)
    (q/background 200) ;; A pleasant grey
    ;; draw nodes
    (doseq [node (lalg/bf-traverse universe)]
      (q/fill 130) ;; Set the fill color to a random grey
      (let [[x y] (lattr/attr universe node :position)]
        (q/ellipse x y system-diameter system-diameter )))
    ;; draw connections TODO: check if we're drawing twice
    (doseq [[n1 n2] (loom/edges universe)]
      (q/fill 100)
      (let [v1 (lattr/attr universe n1 :position)
            v2 (lattr/attr universe n2 :position)]
        (q/line v1 v2)))))

(defn setup-quil! [g]
  (q/frame-rate 60)
  ;; TODO: build a better state pipeline, this is no better
  ;; TODO: than using a global state atom.
  (reset! universe g))

(def size "Display window size" [800 600])

(defn start!
  "Start universe simulation."
  [n-systems]
  (let [universe (let [[w h] size]
                   (gen-universe n-systems :bounds [0 0 w h]))]
    (q/defsketch universe-2d
      :title "Universe 2D"
      :settings #(q/smooth 2) ;; Turn on anti-aliasing
      :setup (partial setup-quil! universe)
      :draw draw!
      :size size)))

(defn -main
  "Generates a random universe representation and renders it."
  [n-systems & _]
  (start! (Integer/parseUnsignedInt n-systems)))

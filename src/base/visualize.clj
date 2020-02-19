(ns rush-hour.base.visualize
  (:gen-class))

(import
 '(java.awt Color Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame JTextField))
(use '[clojure.string :only (replace-first)])

(def grid-dims {:dim 800 :num-sqs 6 :sq-dim 100})

(defn draw-grid
  "Draws a grid with setColor and fillRect calls."
  [im-graph bd draw-x draw-y border-thick sq-dim]
  (if (< draw-x bd)
      (do
        (.setColor im-graph (. Color gray))
        (.fillRect im-graph draw-x draw-y sq-dim sq-dim)
        (draw-grid im-graph bd (+ draw-x sq-dim border-thick) draw-y border-thick sq-dim))
      (when (< (+ draw-y border-thick) bd)
        (draw-grid im-graph bd border-thick (+ draw-y sq-dim border-thick) border-thick sq-dim))))

(defn draw-vehicles
  "Draws the vehicles with setColor and fillRect calls."
  [vs im-graph sq-dim thick]
  (when (some? (first vs))
    (let [[ck {c :color [[bx by] [ex ey]] :location}] (first vs)
          [blk-x blk-y] (mapv (fn [[b e]] (inc (Math/abs (- b e)))) [[bx ex] [by ey]])
          add-extra (fn [nb] (* (dec nb) thick))
          [iw ih] [(* blk-x sq-dim) (* blk-y sq-dim)]
          [w h] (if (> blk-x 1)
                  [(+ iw (add-extra blk-x)) ih]
                  [iw (+ ih (add-extra blk-y))])
          [dx dy] (mapv (fn [p]
                          (+ (* p (+ sq-dim thick)) thick)) [bx by])]
      (.setColor im-graph (Color/decode (replace-first (str c) #":" "#")))
      (.fillRect im-graph dx dy w h)
      (draw-vehicles (rest vs) im-graph sq-dim thick))))

(defn color-frame
  "Invokes sub functions and draws the entire frame."
  [g {d :dim nsqs :num-sqs sq-dim :sq-dim} vs]
  (let [img (new BufferedImage d d (. BufferedImage TYPE_INT_ARGB))
        id (. img (getWidth))
        im-graph (. img (getGraphics))
        thick (Math/round (float (/ (- id (* nsqs sq-dim)) (inc nsqs))))
        on-board-vs (filter #(not= [[] []] ((second %) :location)) (vec vs))
        bd (- id (* 2 thick))]
       (.setColor im-graph (. Color black))
       (.fillRect im-graph 0 0 id id)
       (draw-grid im-graph bd thick thick thick sq-dim)
       (draw-vehicles on-board-vs im-graph sq-dim thick)
       (. g (drawImage img 0 0 nil))
       (. im-graph (dispose))))

(defn panel
  "Returns a new JPanel object with its paint function overriden
   by color-frame."
  [{veh :vehicle}]
  (let [dim (grid-dims :dim)]
             (doto (proxy [JPanel] []
                     (paint [g] (color-frame g grid-dims veh)))
                 (.setPreferredSize (new Dimension dim dim)))))

(defn frame
  "Draws a new frame."
  [bv frame-title]
  (doto
    (new JFrame)
    (.setTitle (str frame-title))
    (-> (.getContentPane) (.add (panel bv)))
    .pack
    .show))

(defn draw-frames
  "Given bvs, draws bvs on a frame and titles them according
   to their index in bvs."
  ([bvs] (draw-frames bvs 0 (count bvs) frame))
  ([[b & bvs] c t frame-func]
   (if (nil? b)
     "done"
     (do
       (frame-func b c)
       (recur bvs (inc c) t frame-func)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

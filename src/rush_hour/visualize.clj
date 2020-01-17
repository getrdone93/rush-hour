(ns rush-hour.visualize
  (:gen-class))

(import
 '(java.awt Color Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))
(use '[clojure.string :only (replace-first)])

(def grid-dims {:dim 800 :num-sqs 6 :sq-dim 100})

(def temp-board {:vehicle {:x {:color :ff0000 :type :car :location [[0 2] [1 2]]}
                                :a {:color :60d700 :type :car :location [[4 0] [5 0]]}
                                :b {:color :ff9e13 :type :car :location [[] []]}
                                :c {:color :05e2f6 :type :car :location [[] []]}
                                :d {:color :ff75da :type :car :location [[] []]}
                                :e {:color :4640bf :type :car :location [[] []]}
                                :f {:color :3b7b39 :type :car :location [[] []]}
                                :g {:color :8a8989 :type :car :location [[] []]}
                                :h {:color :c18862 :type :car :location [[] []]}
                                :i {:color :fff837 :type :car :location [[] []]}
                                :j {:color :532d00 :type :car :location [[] []]}
                                :k {:color :5a9f17 :type :car :location [[] []]}
                                :o {:color :c1bc32 :type :truck :location [[2 0] [2 2]]}
                                :p {:color :b71bff :type :truck :location [[0 3] [2 3]]}
                                :q {:color :256dff :type :truck :location [[5 3] [5 5]]}
                                :r {:color :0eae92 :type :truck :location [[] []]}}})

(defn draw-grid [im-graph bd draw-x draw-y border-thick sq-dim]
  (if (< draw-x bd)
      (do
        (.setColor im-graph (. Color gray))
        (.fillRect im-graph draw-x draw-y sq-dim sq-dim)
        (draw-grid im-graph bd (+ draw-x sq-dim border-thick) draw-y border-thick sq-dim))
      (when (< (+ draw-y border-thick) bd)
        (draw-grid im-graph bd border-thick (+ draw-y sq-dim border-thick) border-thick sq-dim))))

(defn draw-cars [vs im-graph sq-dim thick]
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
      (draw-cars (rest vs) im-graph sq-dim thick))))

(defn color-frame [g {d :dim nsqs :num-sqs sq-dim :sq-dim} {vs :vehicle}]
  (let [img (new BufferedImage d d (. BufferedImage TYPE_INT_ARGB))
        id (. img (getWidth))
        im-graph (. img (getGraphics))
        thick (Math/round (float (/ (- id (* nsqs sq-dim)) (inc nsqs))))
        on-board-vs (filter #(not= [[] []] ((second %) :location)) (vec vs))
        bd (- id (* 2 thick))]
       (.setColor im-graph (. Color black))
       (.fillRect im-graph 0 0 id id)
       (draw-grid im-graph bd thick thick thick sq-dim)
       (draw-cars on-board-vs im-graph sq-dim thick)
       (. g (drawImage img 0 0 nil))
       (. im-graph (dispose))))

(def panel (let [dim (grid-dims :dim)]
             (doto (proxy [JPanel] []
                     (paint [g] (color-frame g grid-dims temp-board)))
                 (.setPreferredSize (new Dimension dim dim)))))

(defn frame [] (doto
                 (new JFrame)
                 (-> (.getContentPane) (.add panel))
                 .pack
                 .show))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

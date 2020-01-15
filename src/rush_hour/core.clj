(ns rush-hour.core
  (:gen-class))

(import
 '(java.awt Color Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

(def grid-dims {:dim 800 :num-sqs 6 :sq-dim 100})
(def board-meta-data {:vehicle {:x {:color :ff0000 :type :car :location [[] []]}
                                :a {:color :60d700 :type :car :location [[] []]}
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
                                :o {:color :c1bc32 :type :truck :location [[] []]}
                                :p {:color :b71bff :type :truck :location [[] []]}
                                :q {:color :256dff :type :truck :location [[] []]}
                                :r {:color :0eae92 :type :truck :location [[] []]}}})

;; (def test-board-meta-data {:vehicle {:x {:color :ff0000 :type :car :location [[2 0] [2 1]]}
;;                                 :a {:color :60d700 :type :car :location [[0 4] [0 5]]}
;;                                 :b {:color :ff9e13 :type :car :location [[] []]}
;;                                 :c {:color :05e2f6 :type :car :location [[] []]}
;;                                 :d {:color :ff75da :type :car :location [[] []]}
;;                                 :e {:color :4640bf :type :car :location [[] []]}
;;                                 :f {:color :3b7b39 :type :car :location [[] []]}
;;                                 :g {:color :8a8989 :type :car :location [[] []]}
;;                                 :h {:color :c18862 :type :car :location [[] []]}
;;                                 :i {:color :fff837 :type :car :location [[] []]}
;;                                 :j {:color :532d00 :type :car :location [[] []]}
;;                                 :k {:color :5a9f17 :type :car :location [[] []]}
;;                                 :o {:color :c1bc32 :type :truck :location [[0 2] [2 2]]}
;;                                 :p {:color :b71bff :type :truck :location [[3 0] [3 2]]}
;;                                 :q {:color :256dff :type :truck :location [[3 5] [5 5]]}
;;                                 :r {:color :0eae92 :type :truck :location [[] []]}}})

(def test-board-meta-data {:vehicle {:x {:color :ff0000 :type :car :location [[] []]}
                                :a {:color :60d700 :type :car :location [[] []]}
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
                                :o {:color :c1bc32 :type :truck :location [[3 5] [5 5]]}
                                :p {:color :b71bff :type :truck :location [[] []]}
                                :q {:color :256dff :type :truck :location [[] []]}
                                :r {:color :0eae92 :type :truck :location [[] []]}}})

(defn gen-board [d]
  (mapv #(vec %)
        (partition d (take (* d d) (repeat {:vehicle :open})))))

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
          map-point (fn [p] (* p (+ sq-dim thick)))
          [dx dy] (mapv #(map-point %) [bx by])]
      (.setColor im-graph (. Color red))
      (.fillRect im-graph dx dy (+ (* blk-y (+ thick sq-dim)) thick) (+ (* blk-x (+ thick sq-dim)) thick))
      (draw-cars (rest vs) im-graph sq-dim thick))))

(defn color-frame [g {d :dim nsqs :num-sqs sq-dim :sq-dim} {vs :vehicle}]
  (let [img (new BufferedImage d d (. BufferedImage TYPE_INT_ARGB))
        id (. img (getWidth))
        im-graph (. img (getGraphics))
        thick (Math/round (float (/ (- id (* nsqs sq-dim)) (inc nsqs))))
        on-board-vs (filter #(not= [[] []] ((second %) :location)) (vec vs))]
       (.setColor im-graph (. Color black))
       (.fillRect im-graph 0 0 id id)
       (draw-grid im-graph (- id (* 2 thick)) thick thick thick sq-dim)
       (draw-cars on-board-vs im-graph sq-dim thick)
       (. g (drawImage img 0 0 nil))
       (. im-graph (dispose))))

(def panel (let [dim (grid-dims :dim)]
             (doto (proxy [JPanel] []
                     (paint [g] (color-frame g grid-dims test-board-meta-data)))
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
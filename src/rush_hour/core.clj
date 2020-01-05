(ns rush-hour.core
  (:gen-class))

(import
 '(java.awt Color Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

(def board-stats {:dim 800 :num-sqs 6 :sq-dim 100})

(defn draw-grid [im-graph bd draw-x draw-y border-thick sq-dim]
  (if (< draw-x bd)
      (do
        (.setColor im-graph (. Color gray))
        (.fillRect im-graph draw-x draw-y sq-dim sq-dim)
        (draw-grid im-graph bd (+ draw-x sq-dim border-thick) draw-y border-thick sq-dim))
      (when (< (+ draw-y border-thick) bd)
        (draw-grid im-graph bd border-thick (+ draw-y sq-dim border-thick) border-thick sq-dim))))

(defn color-frame [g {d :dim nsqs :num-sqs sq-dim :sq-dim}]
  (let [img (new BufferedImage d d (. BufferedImage TYPE_INT_ARGB))
        id (. img (getWidth))
        im-graph (. img (getGraphics))
        thick (Math/round (float (/ (- id (* nsqs sq-dim)) (inc nsqs))))]
       (.setColor im-graph (. Color black))
       (.fillRect im-graph 0 0 id id)
       (draw-grid im-graph (- id (* 2 thick)) thick thick thick sq-dim)
       (. g (drawImage img 0 0 nil))
       (. im-graph (dispose))))

(def panel (let [dim (board-stats :dim)]
             (doto (proxy [JPanel] []
                        (paint [g] (color-frame g board-stats)))
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

(ns rush-hour.core
  (:gen-class))

(import
 '(java.awt Color Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

(def dim 800)

(defn color-frame [g]
  (let [img (new BufferedImage dim dim (. BufferedImage TYPE_INT_ARGB))
        im-graph (. img (getGraphics))]
       (.setColor im-graph (. Color red))
       (.fillRect im-graph 0 0 (. img (getWidth)) (. img (getHeight)))
       (. g (drawImage img 0 0 nil))
       (. im-graph (dispose))))

(def panel (doto (proxy [JPanel] []
                        (paint [g] (color-frame g)))
             (.setPreferredSize (new Dimension dim dim))))

(defn frame [] (doto
                 (new JFrame)
                 (-> (.getContentPane) (.add panel))
                 .pack
                 .show))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

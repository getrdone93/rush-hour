(ns rush-hour.solver.brute-force
  (:require [rush-hour.base.board :as board]))

(defn movable-vehs
  ([board-veh] (movable-vehs board-veh board/move-all-vehicles))
  ([board-veh
    move-av-func] (vec (filter (fn [[ck moves]]
                                 (not (empty? moves)))
                               (move-av-func board-veh)))))

(defn vec-to-map [in-vec]
  (reduce (fn [agg [k v]]
            (assoc agg k v)) {} in-vec))

(defn coords-to-moves
  ([coords]
   (coords-to-moves coords set))
  ([coords out-func]
   (out-func (mapv #(vec %) (partition 2 coords)))))

(defn freed-vehs
  ([new-moves old-moves]
   (freed-vehs new-moves old-moves vec-to-map))
  ([new-moves old-moves vtm-func]
   (clojure.set/difference (set (keys (vtm-func new-moves)))
                           (set (keys (vtm-func old-moves))))))

(defn freed-locs
  ([new-moves old-moves veh-mov-key]
   (freed-locs new-moves old-moves veh-mov-key vec-to-map coords-to-moves))
  ([new-moves old-moves veh-mov-key vtm-func ctm-func]
   (let [[nm om] (map #(vtm-func %) [new-moves old-moves])]
     (clojure.set/difference (set (filter some? (map (fn [k]
                          (when (not (empty? (apply clojure.set/difference (map #(ctm-func (k %)) [nm om]))))
                            k)) (clojure.set/intersection (set (keys nm)) (set (keys om)))))) #{veh-mov-key}))))

(defn optimal-moves
  ([board-veh obj-k]
   (optimal-moves board-veh
                  (movable-vehs board-veh)
                  movable-vehs
                  board/invoke-move
                  freed-vehs
                  freed-locs
                  board/end-state?
                  obj-k))
  ([board-veh
    moves
    move-av-func
    inv-move-func
    fvs-func
    fl-func
    es-func
    obj-k]
   (map (fn [[ck ms]]
          [ck (vec (mapcat #(identity %) (filter (fn [[sp ep]]
                        (let [n-bv (inv-move-func board-veh ck [sp ep])
                              new-moves (move-av-func n-bv)
                              fvs (fvs-func new-moves moves)
                              fls (fl-func new-moves moves ck)
                              end? (es-func n-bv obj-k)]
                          (or (> (apply + (map #(count %) [fvs fls])) 0) end?))) (partition 2 ms))))]) moves)))

;; (defn optimal-paths
;;   ([board-veh] (optimal-paths board-veh board/move-all-vehicles board/invoke-move))
;;   ([{board :board veh :vehicle :as board-veh}
;;     move-av-func
;;     inv-move-func]
;;    (let [opt-moves ]
;;      )))

;; (defn optimal-paths
;;   ([board-veh] (optimal-paths board-veh board/move-all-vehicles board/invoke-move [] []))
;;   ([{board :board veh :vehicle :as board-veh}
;;     move-av-func
;;     inv-move-func
;;     current-path
;;     all-paths]
;;    (let [moves (vec (filter (fn [[ck moves]]
;;                              (not (empty? moves)))
;;                            (move-av-func board-veh)))
;;          _ (println moves)]
;;      ((fn [[[ck [sp ep & rest-points]] & vehs :as am] bv cp ap]
;;         (if (some? ck)
;;           (let [n-ap (optimal-paths (inv-move-func bv ck [sp ep]) move-av-func inv-move-func
;;                                     (conj cp [ck [sp ep]]) ap)]
;;             (if (some? rest-points)
;;               (recur (assoc 0 [ck rest-points]) bv cp n-ap)
;;               (recur vehs bv cp n-ap)))
;;           (if (empty? am)
;;             (conj ap cp)
;;             ap))) moves board-veh current-path all-paths))))

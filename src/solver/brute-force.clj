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
   (filter (fn [[ck mvs]]
             (not (empty? mvs)))
           (map (fn [[ck ms]]
                  [ck (if (= ck obj-k)
                        (vec (take-last 2 ms))
                        (vec (apply concat
                                  (filter (fn [[sp ep]]
                                            (let [n-bv (inv-move-func board-veh ck [sp ep])
                                                  new-moves (move-av-func n-bv)
                                                  end? (es-func n-bv obj-k)]
                                              (or (> (apply + (map #(count %) [(fvs-func new-moves moves)
                                                                               (fl-func new-moves moves ck)])) 0) end?)))
                                          (partition 2 ms)))))]) moves))))

(defn flatten-moves [moves]
  (apply concat (map (fn [[ck mvs]]
                       (let [ms (partition 2 mvs)]
                         (map (fn [k m] 
                                [k (vec m)]) (take (count ms) (repeat ck)) ms))) moves)))

(defn enumerate-paths
  ([board-veh obj-k]
   (enumerate-paths board-veh (flatten-moves (optimal-moves board-veh obj-k)) board/end-state? 
                    optimal-moves flatten-moves board/invoke-move obj-k [] #{} #{board-veh} 0 Integer/MAX_VALUE))
  ([board-veh moves es-func? gen-moves-func flat-func inv-move-func obj-k curr-path all-paths traveled
    depth max-depth]
   (if (> depth max-depth)
     [all-paths max-depth]
     (if (es-func? board-veh obj-k)
       (do
         (println "hit end state, depth: " depth)
         [(conj all-paths curr-path) depth])
       ((fn [[[ck mp] & res] trav cp aps md]
          (if (nil? ck)
            [aps md]
            (let [n-bv (inv-move-func board-veh ck mp)]
              (if (contains? trav n-bv)
                (recur res trav cp aps md)
                (let [[n-aps nd] (enumerate-paths n-bv (flat-func (gen-moves-func n-bv obj-k)) es-func?
                                             gen-moves-func flat-func inv-move-func obj-k (conj cp [ck mp])
                                             aps (conj trav n-bv) (inc depth) md)]
                  (recur res trav cp (clojure.set/union aps n-aps) nd))))))
        moves traveled curr-path all-paths max-depth)))))

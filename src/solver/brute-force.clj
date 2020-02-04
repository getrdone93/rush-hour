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
             (not (empty? mvs))) (map (fn [[ck ms]]
          [ck (vec (apply concat
                           (filter (fn [[sp ep]]
                                     (let [n-bv (inv-move-func board-veh ck [sp ep])
                                           new-moves (move-av-func n-bv)
                                           end? (es-func n-bv obj-k)]
                                       (or (> (apply + (map #(count %) [(fvs-func new-moves moves)
                                                                        (fl-func new-moves moves ck)])) 0) end?)))
                                   (partition 2 ms))))]) moves))))

(defn optimal-paths
  ([board-veh obj-k]
   (optimal-paths
    board-veh
    (optimal-moves board-veh obj-k)
    board/end-state?
    optimal-moves
    board/invoke-move
    obj-k
    []
    []))
  ([board-veh
    opt-moves
    es-func?
    opt-move-func
    inv-move-func
    obj-k
    curr-path
    opt-paths]
   ((fn [[[ck [s1 e1 & res-ps]] & res-vehs] cp ops]
      (if (nil? ck)
        (if (es-func? board-veh)
          (conj ops cp)
          ops)
        (let [n-aps (optimal-paths (inv-move-func board-veh ck [s1 e1]) (opt-move-func board-veh obj-k)
                              es-func? opt-move-func inv-move-func obj-k (conj cp [ck [s1 e1]]) ops)]
          (cond
            (not (empty? res-ps)) (recur (conj res-vehs [ck (vec res-ps)]) cp ops)
            (not (empty? res-vehs)) (recur res-vehs cp ops)
            true ops)))) opt-moves curr-path opt-paths)))

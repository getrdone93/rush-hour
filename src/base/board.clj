(ns rush-hour.base.board)

(defn gen-board [d]
  (mapv #(vec %)
        (partition d (take (* d d) (repeat {:vehicle :open})))))

(defn six-board [] (gen-board 6))

(defn bounds-check [dim nums]
  (every? true? (map #(and (> % -1) (< % dim))
                     (flatten nums))))

(defn space-check [board veh-key [bx by] [ex ey]]
  (let [n-bg-sp (-> board (nth bx) (nth by) :vehicle)
        n-end-sp (-> board (nth ex) (nth ey) :vehicle)]
    (or (and (= n-bg-sp :open)
             (= n-end-sp veh-key))
        (and (= n-bg-sp veh-key)
             (= n-end-sp :open)))))

(defn move
  ([{board :board
     {{loc :location} mv-veh-k} :vehicle}
    mv-veh-k
    mv-func] (move board loc mv-veh-k mv-func bounds-check space-check))
  ([board
    [[bx by] [ex ey] :as loc]
    mv-veh-k
    mv-func
    bc-func
    sc-func]
   (let [dim (count board)
         [[n-bx n-by] [n-ex n-ey] :as n-loc] (if (= bx ex)
                                               [[bx (mv-func by)] [ex (mv-func ey)]]
                                               [[(mv-func bx) by] [(mv-func ex) ey]])]
     (if (bc-func dim [n-bx n-by n-ex n-ey])
       (if (sc-func board mv-veh-k [n-bx n-by] [n-ex n-ey])
         [n-loc true]
         [loc false])
       [loc false]))))

(defn right-or-down [board-veh mv-veh-k]
  (move board-veh mv-veh-k inc))

(defn left-or-up [board-veh mv-veh-k]
  (move board-veh mv-veh-k dec))

(defn enumerate-coords [[bx by] [ex ey]]
  (let [x-rng (range bx (inc ex))
        y-rng (range by (inc ey))
        [xs ys] (if (= (count x-rng) 1)
                  [(repeat (count y-rng) (first x-rng)) y-rng]
                  [x-rng (repeat (count x-rng) (first y-rng))])]
    (mapv (fn [x y]
            [x y]) xs ys)))

(defn vehicle-locs
  ([vehicles] (vehicle-locs vehicles enumerate-coords))
  ([vehicles enum-func]
   (mapv (fn [[ck {[[bx by :as bg] [ex ey :as ed]] :location}]]
           (mapv (fn [loc]
                   [ck loc]) (enum-func bg ed))) vehicles)))

(defn sync-meta
  ([{vhs :vehicle}] (sync-meta (six-board) (vehicle-locs vhs)))
  ([board veh-locs]
  (let [flat-vl (apply concat veh-locs)]
    (reduce (fn [agg [ck [x y]]]
              (assoc-in agg [x y] {:vehicle ck})) board flat-vl))))

(defn invoke-move [{board :board vehs :vehicle} mv-veh-k n-loc]
  (let [n-vehs (assoc-in vehs [mv-veh-k :location] n-loc)]
      {:board (sync-meta {:vehicle n-vehs :board board})
       :vehicle n-vehs}))

(defn one-direct-veh-move
  ([board-veh mv-veh-k move-func] (one-direct-veh-move board-veh mv-veh-k move-func []))
  ([{board :board
     {{loc :location} mv-veh-k :as vehs} :vehicle :as board-veh}
    mv-veh-k
    move-func
    res]
   (let [[loc val] (move-func board-veh mv-veh-k)]
     (if val
       (let [{{{new-loc :location} mv-veh-k} :vehicle
              :as new-board-veh} (invoke-move board-veh mv-veh-k loc)]
         (one-direct-veh-move new-board-veh mv-veh-k move-func (into res new-loc)))
       res))))

(defn all-direct-veh-move
  ([board-veh mv-veh-k] (all-direct-veh-move board-veh mv-veh-k left-or-up right-or-down))
  ([board-veh mv-veh-k left-move-func right-move-func]
   (vec (concat (one-direct-veh-move board-veh mv-veh-k left-move-func)
                   (one-direct-veh-move board-veh mv-veh-k right-move-func)))))

(defn move-all-vehicles
  ([board-veh] (move-all-vehicles board-veh all-direct-veh-move))
  ([{board :board vehs :vehicle :as board-veh} all-dvm-func]
   (reduce (fn [agg ck]
             (assoc agg ck (all-dvm-func board-veh ck))) {} (keys vehs))))

(defn end-state? [{{{loc :location} obj-k} :vehicle}
                       obj-k]
  (= loc [[4 2] [5 2]]))

(defn turing-machine
  ([start-state actions obj-k]
   (turing-machine start-state actions invoke-move end-state? obj-k []))
  ([current-state [[ck mp] & actions] inv-move-func end-state-func obj-k states]
   (if (and (nil? ck) (nil? actions))
     (if (end-state-func current-state obj-k)
       [true states]
       [false states])
     (let [next-state (inv-move-func current-state ck mp)]
       (turing-machine next-state actions inv-move-func end-state-func obj-k (conj states next-state))))))

(defn optimal-paths [board-veh obj-k enumerate-func]
  (let [[valid invalid] (partition-by #(true? (first %))
                          (map #(turing-machine board-veh % obj-k)
                               (first (partition-by count
                                                    (sort-by count
                                                             (enumerate-func board-veh obj-k))))))]
    {:valid (mapv second valid) :invalid (mapv second invalid)}))

(def base-veh {:x {:color :ff0000 :type :car :location [[] []]}
               :a {:color :60d700 :type :car :location [[] []]}
               :b {:color :ff9e13 :type :car :location [[] []]}
               :c {:color :05e2f6 :type :car :location [[] []]}
               :d {:color :ff75da :type :car :location [[] []]}
               :e {:color :4640bf :type :car :location [[] []]}
               :f {:color :3b7b39 :type :car :location [[] []]}
               :g {:color :ebeef2 :type :car :location [[] []]}
               :h {:color :c18862 :type :car :location [[] []]}
               :i {:color :fff837 :type :car :location [[] []]}
               :j {:color :532d00 :type :car :location [[] []]}
               :k {:color :5a9f17 :type :car :location [[] []]}
               :o {:color :c1bc32 :type :truck :location [[] []]}
               :p {:color :b71bff :type :truck :location [[] []]}
               :q {:color :256dff :type :truck :location [[] []]}
               :r {:color :0eae92 :type :truck :location [[] []]}})

(def card-1-veh {:x {:color :ff0000 :type :car :location [[0 2] [1 2]]}
                 :a {:color :60d700 :type :car :location [[4 0] [5 0]]}
                 :o {:color :c1bc32 :type :truck :location [[2 0] [2 2]]}
                 :p {:color :b71bff :type :truck :location [[0 3] [2 3]]}
                 :q {:color :256dff :type :truck :location [[5 3] [5 5]]}})

(def card-2-veh {:o {:color :c1bc32 :type :truck :location [[2 0] [2 2]]}
                 :x {:color :ff0000 :type :car :location [[3 2] [4 2]]}
                 :p {:color :b71bff :type :truck :location [[5 1] [5 3]]}
                 :q {:color :256dff :type :truck :location [[0 3] [2 3]]}
                 :a {:color :60d700 :type :car :location [[3 3] [3 4]]}
                 :b {:color :ff9e13 :type :car :location [[4 4] [5 4]]}})

(def card-40-veh {:a {:color :60d700 :type :car :location [[0 0] [0 1]]}
                  :b {:color :ff9e13 :type :car :location [[1 0] [2 0]]}
                  :o {:color :c1bc32 :type :truck :location [[3 0] [5 0]]}
                  :c {:color :05e2f6 :type :car :location [[2 1] [3 1]]}
                  :d {:color :ff75da :type :car :location [[4 1] [4 2]]}
                  :x {:color :ff0000 :type :car :location [[0 2] [1 2]]}
                  :e {:color :4640bf :type :car :location [[2 2] [2 3]]}
                  :g {:color :ebeef2 :type :car :location [[3 3] [4 3]]}
                  :f {:color :3b7b39 :type :car :location [[0 3] [1 3]]}
                  :p {:color :b71bff :type :truck :location [[5 3] [5 5]]}
                  :h {:color :c18862 :type :car :location [[1 4] [2 4]]}
                  :q {:color :256dff :type :truck :location [[0 5] [2 5]]}
                  :i {:color :fff837 :type :car :location [[3 4] [3 5]]}})

(def problem-1 {:vehicle card-1-veh :board (sync-meta {:board (six-board) :vehicle card-1-veh})})
(def problem-2 {:vehicle card-2-veh :board (sync-meta {:board (six-board) :vehicle card-2-veh})})
(def problem-40 {:vehicle card-40-veh :board (sync-meta {:board (six-board) :vehicle card-40-veh})})





(ns rush-hour.board)

(defn gen-board [d]
  (mapv #(vec %)
        (partition d (take (* d d) (repeat {:vehicle :open})))))

(defn bounds-check [dim nums]
  (every? true? (map #(and (> % -1) (< % dim))
                     (flatten nums))))

(defn move [{board :board
             {{[[bx by] [ex ey] :as loc] :location} mv-veh-k} :vehicle}
            mv-veh-k
            mv-func
            bc-func]
  (let [dim (count board)
        [[n-bx n-by] [n-ex n-ey] :as n-loc] (if (= bx ex)
                                              [[bx (mv-func by)] [ex (mv-func ey)]]
                                              [[(mv-func bx) by] [(mv-func ex) ey]])]
    (if (bc-func dim [n-bx n-by n-ex n-ey])
      (let [n-bg-sp (-> board (nth n-bx) (nth n-by) :vehicle)
            n-end-sp (-> board (nth n-ex) (nth n-ey) :vehicle)]
        (if (or (and (= n-bg-sp :open)
                     (= n-end-sp mv-veh-k))
                (and (= n-bg-sp mv-veh-k)
                     (= n-end-sp :open)))
          n-loc
          loc))
      loc)))

(defn enumerate-coords [[bx by] [ex ey]]
  (let [x-rng (range bx (inc ex))
        y-rng (range by (inc ey))
        [xs ys] (if (= (count x-rng) 1)
                  [(repeat (count y-rng) (first x-rng)) y-rng]
                  [x-rng (repeat (count x-rng) (first y-rng))])]
    (mapv (fn [x y]
            [x y]) xs ys)))

(defn vehicle-locs [{vhs :vehicle} enum-func]
  (mapv (fn [[ck {[[bx by :as bg] [ex ey :as ed]] :location}]]
          (mapv (fn [loc]
                 [ck loc]) (enum-func bg ed))) vhs))

(defn sync-meta [{bd :board} veh-locs]
  (let [flat-vl (mapcat #(identity %) veh-locs)]
    (reduce (fn [agg [ck [x y]]]
              (assoc-in agg [x y] {:vehicle ck})) bd flat-vl)))

(def base-veh {:x {:color :ff0000 :type :car :location [[] []]}
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
               :r {:color :0eae92 :type :truck :location [[] []]}})

(def card-1-veh {:x {:color :ff0000 :type :car :location [[0 2] [1 2]]}
                 :a {:color :60d700 :type :car :location [[4 0] [5 0]]}
                 :o {:color :c1bc32 :type :truck :location [[2 0] [2 2]]}
                 :p {:color :b71bff :type :truck :location [[0 3] [2 3]]}
                 :q {:color :256dff :type :truck :location [[5 3] [5 5]]}})

(def problem-1 {:vehicle card-1-veh :board (sync-meta {:board (gen-board 6)}
                                                      (vehicle-locs {:vehicle card-1-veh} enumerate-coords))})

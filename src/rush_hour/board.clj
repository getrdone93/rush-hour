(ns rush-hour.board)

(def base {:vehicle {:x {:color :ff0000 :type :car :location [[] []]}
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

(def card-1 {:vehicle {:x {:color :ff0000 :type :car :location [[0 2] [1 2]]}
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

(defn gen-board [d]
  (mapv #(vec %)
        (partition d (take (* d d) (repeat {:vehicle :open})))))



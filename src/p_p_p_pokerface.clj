(ns p-p-p-pokerface)

(def replacement {\T 10, \J 11, \Q 12, \K 13, \A 14})
(defn rank [card]
   (let [[r _] card]
    (cond
       (Character/isDigit r) (Integer/valueOf (str r))
       :else (replacement r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
    (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
      (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (== 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (range 2 4)))

(defn two-pairs? [hand]
  (let [m (sort (vals (frequencies (map rank hand))))]
    (or (= m [1 2 2]) (= m [1 4]))))

(defn straight? [hand]
  (let [m (sort (map (fn [x] (Integer/valueOf x))
       (keys (frequencies (replace {14 1} (map rank hand))))))
   r (range (first m) (+ (first m) 5))] (= m r)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers? #{ [high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4]  [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
      ] (apply max (map second (filter (fn [x] ((first x) hand)) (seq checkers?))))))

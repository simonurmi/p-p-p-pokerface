(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/parseInt (str rank))
      (values rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [sorted (sort (vals (frequencies (map rank hand))))]
    (= [2 3] sorted)))

(defn two-pairs? [hand]
  (let [sorted (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] sorted) (= [1 4] sorted))))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        low-ace (sort (replace {14 1} sorted))
        fst (first sorted)
        lst (last sorted)
        fst-low (first low-ace)
        lst-low (last low-ace)]
    (or (= sorted (range fst (+ 1 lst))) (= low-ace (range fst-low (+ 1 lst-low))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        check (fn [checker] ((first checker) hand))
        hands (filter check checkers)
        scores (map second hands)]
    (apply max scores)))

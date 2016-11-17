(ns xmas)

(def family
  [{:name "Kathy, Sr."}
   {:name "Charles"}
   {:name "Molly"}
   {:name "Paul" :spouse "Jane"}
   {:name "Jane" :spouse "Paul"}
   {:name "Mari" :spouse "Clem"}
   {:name "Clem" :spouse "Mari"}
   {:name "Jackie" :spouse "Matt"}
   {:name "Matt" :spouse "Jackie"}
   {:name "Kathy" :spouse "Cliff"}
   {:name "Cliff" :spouse "Kathy"}
   {:name "Colleen"}])


(defn make-round []
  (let [first-half (->> family
                        shuffle
                        (partition 2)
                        (map (fn [pair]
                               {:giver (first pair)
                                :receiver (second pair)})))
        second-half (map (fn [pair] {:giver (first pair)
                                     :receiver (second pair)})
                         (partition 2 (interleave
                                       (shuffle (map :receiver first-half))
                                       (shuffle (map :giver first-half)))))]
    (concat first-half second-half)))


(defn with-spouse? [pair]
  (or (= (get-in pair [:giver :name])
         (get-in pair [:receiver :spouse]))
      (= (get-in pair [:receiver :name])
         (get-in pair [:giver :spouse]))))


(defn reciprocal? [round]
  (reduce (fn [accum item]
            (let [recip (first (filter #(and (= (:giver item)
                                                (:receiver %))
                                             (= (:giver %)
                                                (:receiver item)))
                                       (remove #(= item %) round)))]
              (if recip
                (cons recip accum)
                accum))) [] round))


(defn already-paired? [previous-rounds current-round]
  (remove nil? (map (partial some (set current-round))
                    previous-rounds)))


(defn xchange []
  (loop [exchanges []
         attempts 0]
    (if (or (= 8 (count exchanges))
            (= 1300000 attempts))
      (do
        (println "This many attempts: " attempts)
        (map (fn [exchange]
               (map (fn [pair]
                      {:giver (get-in pair [:giver :name])
                       :receiver (get-in pair [:receiver :name])})
                    exchange))
             exchanges))
      (let [this-round (make-round)
            matching-round (when (and (not-any? with-spouse? this-round)
                                      (empty? (reciprocal? this-round)))
                             this-round)
            result (if (empty? (already-paired? exchanges matching-round))
                     (cons matching-round exchanges)
                     exchanges)]
        (recur (remove nil? result) (inc attempts))))))

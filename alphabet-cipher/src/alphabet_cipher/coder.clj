(ns alphabet-cipher.coder)

(def a-z (map char (range 97 123)))

(defn rotate [coll] (concat (rest coll) [(first coll)]))

(defn build-chart [coll]
  (loop [coll coll
         previous coll
         result [coll]]
    (if-not (seq coll)
      result
      (let [rotation (rotate previous)]
        (recur (rest coll) rotation (conj result rotation))))))

(def chart
  (let [zip-with-a-z (partial zipmap a-z)]
    (zip-with-a-z (map zip-with-a-z (build-chart a-z)))))

(defn padded-key [key]
  (cycle (seq key)))

(defn pad-key [keyword message]
  (take (count message) (padded-key keyword)))

(defn encode-fn [coll]
  (for [pair coll]
    (get-in chart pair)))

(defn decode-fn [coll]
  (for [[k v] coll]
    (ffirst (filter #(= (val %) v) (chart k)))))

(defn *code [keyword message f]
  (let [key (pad-key keyword message)
        encoding (interleave key message)
        pairs (partition 2 encoding)]
    (apply str (f pairs))))

(defn encode [keyword message]
  (*code keyword message encode-fn))

(defn decode [keyword message]
  (*code keyword message decode-fn))

(ns clojure.data.codec.test-hex
  (:import org.apache.commons.codec.binary.Hex
           [java.io ByteArrayInputStream ByteArrayOutputStream])
  (:use clojure.test
        clojure.data.codec.hex))

(set! *warn-on-reflection* true)

(defn rand-bytes [n]
  (->> #(byte (- (rand-int 256) 128))
    repeatedly
    (take n)
    (byte-array)))

(defn chars->bytes [^chars cs]
  (.getBytes (String. cs)))

(deftest enc-correctness
  (doseq [n (concat (range 1 6) (range 1001 1006))]
    (is (let [input (rand-bytes n)
              a1 (encode input)
              a2 (Hex/encodeHex input)]
          (= (into [] a1) (into [] (chars->bytes a2)))))))

(deftest offset-enc-correctness
  (doseq [n (concat (range 1 6) (range 1001 1006))]
    (doseq [off (range 1 (min n 5))]
      (is (let [input (rand-bytes n)
                len (- n off)
                a1 (encode input off len)
                input2 (byte-array len)
                _ (System/arraycopy input off input2 0 len)
                a2 (Hex/encodeHex input2)]
            (= (into [] a1) (into [] (chars->bytes a2))))))))

(deftest buffer-enc-correctness
  (doseq [n (concat (range 1 6) (range 1001 1006))]
    (doseq [excess-buf-len (range 1 10)]
      (is (let [input (rand-bytes n)
                output (byte-array (+ (* n 2) excess-buf-len))
                _ (encode! input 0 n output)
                a2 (Hex/encodeHex input)]
            (= (into [] (take (* n 2) output)) (into [] (chars->bytes a2))))))))

(deftest dec-correctness
  (doseq [n (concat (range 1 6) (range 1001 1006))]
    (is (let [orig (rand-bytes n)
              enc (encode orig)
              deco (decode enc)]
          (= (into [] deco) (into [] orig))))))

(deftest offset-dec-correctness
  (doseq [n (concat (range 1 6) (range 1001 1006))]
    (doseq [off (range 1 (min n 5))]
      (is (let [orig (rand-bytes n)
                enc (byte-array (concat (repeat off (byte 0)) (encode orig)))
                deco (decode enc off (- (alength enc) off))]
            (= (into [] deco) (into [] orig)))))))

(deftest buffer-dec-correctness
  (doseq [n (concat (range 1 6) (range 1001 1006))]
    (doseq [excess-buf-len (range 1 10)]
      (is (let [orig (rand-bytes n)
                enc (encode orig)
                deco (byte-array (+ n excess-buf-len))
                _ (decode! enc 0 (alength ^bytes enc) deco)]
            (= (into [] (take n deco)) (into [] orig)))))))

(deftest fit-encoding-transfer
  (let [raw (rand-bytes 3)
        in (ByteArrayInputStream. raw)
        out (ByteArrayOutputStream.)]
    (encoding-transfer in out :buffer-size 3)
    (is (= (into [] (.toByteArray out)) (into [] (encode raw))))))

(deftest split-encoding-transfer
  (let [raw (rand-bytes 4)
        in (ByteArrayInputStream. raw)
        out (ByteArrayOutputStream.)]
    (encoding-transfer in out :buffer-size 3)
    (is (= (into [] (.toByteArray out)) (into [] (encode raw))))))

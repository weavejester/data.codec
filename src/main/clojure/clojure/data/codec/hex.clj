;; by James Reeves
;; March 29, 2014

;; Copyright (c) James Reeves, March 2014. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns ^{:author "James Reeves"
      :doc "Functions for working with hex encodings."}
  clojure.data.codec.hex
  (:import [java.io InputStream OutputStream]))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(def ^:private ^"[B" enc-bytes
  (byte-array (map (comp byte int) "0123456789abcdef")))

(def ^:private ^"[B" dec-bytes
  (let [^bytes ba (byte-array (inc (apply max enc-bytes)))]
    (doseq [[idx enc] (map-indexed vector enc-bytes)]
      (aset ba enc (byte idx)))
    ba))

(defn decode!
  "Reads from the input byte array for the specified length starting at the offset
   index, and hex decodes into the output array starting at index 0. Returns the
   length written to output.

   Note: length must be a multiple of 2."
  [^bytes input ^long offset ^long length ^bytes output]
  (let [end (+ offset length)]
    (loop [i offset, j 0]
      (if (< i end)
        (let [n1 (aget dec-bytes (aget input i))
              n2 (aget dec-bytes (aget input (inc i)))]
          (aset output j (byte (bit-or (bit-shift-left n1 4) n2)))
          (recur (+ i 2) (inc j)))))))

(defn decode
  "Returns a hex decoded byte array.

  Note: length must be a multiple of 2."
  ([^bytes input]
    (decode input 0 (alength input)))
  ([^bytes input ^long offset ^long length]
    (let [dest (byte-array (/ length 2))]
      (decode! input offset length dest)
      dest)))

(defn encode!
  "Reads from the input byte array for the specified length starting at the offset
   index, and hex encodes into the output array starting at index 0. Returns the
   length written to output."
  [^bytes input ^long offset ^long length ^bytes output]
  (let [end (+ offset length)]
    (loop [i offset, j 0]
      (if (< i end)
        (let [b (aget input i)]
          (aset output j (aget enc-bytes (bit-shift-right (bit-and 0xf0 b) 4)))
          (aset output (inc j) (aget enc-bytes (bit-and 0x0f b)))
          (recur (inc i) (+ j 2)))))))

(defn encode
  "Returns a hex encoded byte array."
  ([^bytes input]
    (encode input 0 (alength input)))
  ([^bytes input ^long offset ^long length]
    (let [dest (byte-array (* length 2))]
      (encode! input offset length dest)
      dest)))

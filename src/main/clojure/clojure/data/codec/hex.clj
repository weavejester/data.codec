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
  ^long [^bytes input ^long offset ^long length ^bytes output]
  (let [end (+ offset length)]
    (loop [i offset, j 0]
      (if (< i end)
        (let [n1 (long (aget dec-bytes (aget input i)))
              n2 (long (aget dec-bytes (aget input (inc i))))
              b  (bit-or (bit-shift-left n1 4) n2)]
          (aset output j (byte b))
          (recur (+ i 2) (inc j)))
        j))))

(defn decode
  "Returns a hex decoded byte array.

  Note: length must be a multiple of 2."
  ([^bytes input]
    (decode input 0 (alength input)))
  ([^bytes input ^long offset ^long length]
    (let [dest (byte-array (quot length 2))]
      (decode! input offset length dest)
      dest)))

(defn encode!
  "Reads from the input byte array for the specified length starting at the offset
   index, and hex encodes into the output array starting at index 0. Returns the
   length written to output."
  ^long [^bytes input ^long offset ^long length ^bytes output]
  (let [end (+ offset length)]
    (loop [i offset, j 0]
      (if (< i end)
        (let [b  (long (aget input i))
              n1 (bit-shift-right (bit-and 0xf0 b) 4)
              n2 (bit-and 0x0f b)]
          (aset output j (aget enc-bytes n1))
          (aset output (inc j) (aget enc-bytes n2))
          (recur (inc i) (+ j 2)))
        j))))

(defn encode
  "Returns a hex encoded byte array."
  ([^bytes input]
    (encode input 0 (alength input)))
  ([^bytes input ^long offset ^long length]
    (let [dest (byte-array (* length 2))]
      (encode! input offset length dest)
      dest)))

(defn encoding-transfer
  "Hex encodes from input-stream to output-stream. Returns nil or throws IOException.

  Options are key/value pairs and may be one of
    :buffer-size  read buffer size to use; default is 4096."
  [^InputStream input-stream ^OutputStream output-stream & opts]
  (let [opts (if opts (apply hash-map opts))
        in-size (:buffer-size opts 4096)
        out-size (* in-size 2)
        in-buf (byte-array in-size)
        out-buf (byte-array out-size)]
    (loop []
      (let [in-size (.read input-stream in-buf)]
        (when (pos? in-size)
          (let [out-size (encode! in-buf 0 in-size out-buf)]
            (.write output-stream out-buf 0 out-size)
            (recur)))))))

(defn- decode-buffer-size ^long [opts ^long default]
  (if-let [in-size (:buffer-size opts)]
    (if (even? in-size)
      in-size
      (throw (IllegalArgumentException. "Buffer size must be a multiple of 2.")))
    default))

(defn decoding-transfer
  "Hex decodes from input-stream to output-stream. Returns nil or throws IOException.

  Options are key/value pairs and may be one of
    :buffer-size  read buffer size to use; default is 4096."
  [^InputStream input-stream ^OutputStream output-stream & opts]
  (let [opts (if opts (apply hash-map opts))
        in-size (decode-buffer-size opts 4096)
        out-size (quot in-size 2)
        in-buf (byte-array in-size)
        out-buf (byte-array out-size)]
    (loop [carry nil]
      (let [buf-size (if carry
                       (do (aset in-buf 0 (byte carry))
                           (inc (.read input-stream in-buf 1 (dec in-size))))
                       (.read input-stream in-buf))]
        (if (pos? buf-size)
          (if (odd? buf-size)
            (let [out-size (decode! in-buf 0 (dec buf-size) out-buf)]
              (.write output-stream out-buf 0 out-size)
              (recur (aget in-buf (dec buf-size))))
            (let [out-size (decode! in-buf 0 buf-size out-buf)]
              (.write output-stream out-buf 0 out-size)
              (recur nil))))))))

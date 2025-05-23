;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.fills
  (:require
   #?(:cljs [app.common.weak-map :as weak-map])
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.common.buffer :as buf]))

;; FIXME: reivisit
(def ^:const MAX-GRADIENT-STOPS 12)
(def ^:const MAX-FILLS 8)

(def ^:const GRADIENT-STOP-SIZE 8)
(def ^:const GRADIENT-BYTE-SIZE 156)
(def ^:const SOLID-BYTE-SIZE 4)
(def ^:const IMAGE-BYTE-SIZE 28)
(def ^:const METADATA-BYTE-SIZE 36)
(def ^:const FILL-BYTE-SIZE
  (+ 4 (max GRADIENT-BYTE-SIZE
            IMAGE-BYTE-SIZE
            SOLID-BYTE-SIZE)))

(def xf:take-stops
  (take MAX-GRADIENT-STOPS))

(def xf:take-fills
  (take MAX-FILLS))

(defn- in-range?
  [size i]
  (and (< i size) (>= i 0)))

(defn print-binary
  [num]
  #?(:cljs (println (.padStart
                     (.toString num 2)
                     8
                     "0"))))

(defn- hex->rgb
  "Encode an hex string as rgb (int32)"
  [hex]
  (let [hex    (subs hex 1)
        result #?(:clj (Integer/parseInt hex 16)
                  :cljs (js/parseInt hex 16))]
    ;; (prn "hex->rgb" hex "->" result)
    result))

(defn- rgb->rgba
  "Use the first 2 bytes of in32 for encode the alpha channel"
  [n alpha]
  (let [result (mth/floor (* alpha 0xff))
        ;; _ (prn "PREV" 1 result)
        result (unchecked-int result)
        ;; _ (prn "PREV" 2 result)
        result (bit-shift-left result 24)
        ;; _ (prn "PREV" 2 result)
        result (bit-or result n)
        ;; _ (prn "PREV" 4 result)
        ]
    ;; (prn "rgb->rgba" n alpha "->" result)
    result))

(defn- get-color-hex
  [n]
  (let [n (bit-and n 0x00ffffff)]
    (str "#" #?(:clj (String/format "%x" (into-array Object [n]))
                :cljs (let [color (.toString n 16)]
                        (.padStart color 6 "0"))))))

(defn- get-color-alpha
  [rgb]
  (let [n (bit-and rgb 0xff000000)
        n (unsigned-bit-shift-right n 24)]
    ;; (println "----")
    ;; (print-binary (unsigned-bit-shift-right (bit-or rgb 0x00000000) 0))
    ;; (print-binary n)
    (mth/precision (/ (float n) 0xff) 2)))

(defn- write-solid-fill
  [offset buffer color alpha]
  (buf/write-byte buffer (+ offset 0) 0x00)
  (buf/write-int  buffer (+ offset 4)
                  (-> (hex->rgb color)
                      (rgb->rgba alpha)))
  (+ offset FILL-BYTE-SIZE))

(defn- write-gradient-fill
  [offset buffer gradient opacity]
  (let [start-x (:start-x gradient)
        start-y (:start-y gradient)
        end-x   (:end-x gradient)
        end-y   (:end-y gradient)
        width   (:width gradient 0)
        stops   (into [] xf:take-stops (:stops gradient))
        type    (if (= (:type gradient) :linear)
                  0x01
                  0x02)]

    (buf/write-byte  buffer (+ offset 0)  type)
    (buf/write-float buffer (+ offset 4)  start-x)
    (buf/write-float buffer (+ offset 8)  start-y)
    (buf/write-float buffer (+ offset 12) end-x)
    (buf/write-float buffer (+ offset 16) end-y)
    (buf/write-float buffer (+ offset 20) opacity)
    (buf/write-float buffer (+ offset 24) width)
    (buf/write-byte  buffer (+ offset 28) (count stops))

    (loop [stops   (seq stops)
           offset' (+ offset 32)]
      (if-let [stop (first stops)]
        (let [color (-> (hex->rgb (:color stop))
                        (rgb->rgba (:opacity stop 1)))]
          ;; NOTE: we write the color as signed integer but on rust
          ;; side it will be read as unsigned, on the end the binary
          ;; repr of the data is the same independently on how it is
          ;; interpreted
          (buf/write-int   buffer (+ offset' 0) color)
          (buf/write-float buffer (+ offset' 4) (:offset stop))
          (recur (rest stops)
                 (+ offset' GRADIENT-STOP-SIZE)))
        (+ offset FILL-BYTE-SIZE)))))

(defn- write-image-fill
  [offset buffer opacity image]
  (let [image-id (get image :id)
        image-width (get image :width)
        image-height (get image :height)]
    (buf/write-byte  buffer (+ offset  0) 0x03)
    (buf/write-uuid  buffer (+ offset  4) image-id)
    (buf/write-float buffer (+ offset 20) opacity)
    (buf/write-int   buffer (+ offset 24) image-width)
    (buf/write-int   buffer (+ offset 28) image-height)
    (+ offset FILL-BYTE-SIZE)))

(defn- write-metadata
  [offset buffer fill]
  (let [ref-id   (:fill-color-ref-id fill)
        ref-file (:fill-color-ref-file fill)
        mtype    (dm/get-in fill [:fill-image :mtype])]

    (when mtype
      (let [val (case mtype
                  "image/jpeg" 0x01
                  "image/png"  0x02
                  "image/gif"  0x03
                  "image/webp"  0x04
                  "image/svg+xml" 0x05)]
        (buf/write-short buffer (+ offset 2) val)))

    (if (and ref-file ref-id)
      (do
        (buf/write-byte buffer (+ offset 0) 0x01)
        (buf/write-uuid buffer (+ offset 4) ref-file)
        (buf/write-uuid buffer (+ offset 20) ref-id))
      (do
        (buf/write-byte buffer (+ offset 0) 0x00)))))

(defn- read-stop
  [buffer offset]
  (let [rgba (buf/read-int buffer (+ offset 0))
        soff (buf/read-float buffer (+ offset 4))]
    {:color (get-color-hex rgba)
     :opacity (get-color-alpha rgba)
     :offset soff}))

(defn- read-fill
  "Read segment from binary buffer at specified index"
  [dbuffer mbuffer index]
  (let [doffset (+ 4 (* index FILL-BYTE-SIZE))
        moffset (* index METADATA-BYTE-SIZE)
        type    (buf/read-byte dbuffer doffset)
        refs?   (buf/read-bool mbuffer (+ moffset 0))
        fill    (case type
                  0
                  (let [rgba (buf/read-int dbuffer (+ doffset 4))]
                    {:fill-color (get-color-hex rgba)
                     :fill-opacity (get-color-alpha rgba)})

                  (1 2)
                  (let [start-x (buf/read-float dbuffer (+ doffset 4))
                        start-y (buf/read-float dbuffer (+ doffset 8))
                        end-x   (buf/read-float dbuffer (+ doffset 12))
                        end-y   (buf/read-float dbuffer (+ doffset 16))
                        alpha   (buf/read-float dbuffer (+ doffset 20))
                        width   (buf/read-float dbuffer (+ doffset 24))
                        stops   (buf/read-byte  dbuffer (+ doffset 28))
                        type    (if (= type 1)
                                  :linear
                                  :radial)
                        stops   (loop [index  0
                                       result []]
                                  (if (< index stops)
                                    (recur (inc index)
                                           (conj result (read-stop dbuffer (+ doffset 32 (* GRADIENT-STOP-SIZE index)))))
                                    result))]

                    {:fill-opacity alpha
                     :fill-color-gradient {:start-x start-x
                                           :start-y start-y
                                           :end-x end-x
                                           :end-y end-y
                                           :width width
                                           :stops stops
                                           :type type}})

                  3
                  (let [id     (buf/read-uuid  dbuffer (+ doffset 4))
                        alpha  (buf/read-float dbuffer (+ doffset 20))
                        width  (buf/read-int   dbuffer (+ doffset 24))
                        height (buf/read-int   dbuffer (+ doffset 28))
                        mtype  (buf/read-short mbuffer (+ moffset 2))
                        mtype  (case mtype
                                 0x01 "image/jpeg"
                                 0x02 "image/png"
                                 0x03 "image/gif"
                                 0x04 "image/webp"
                                 0x05 "image/svg+xml")]
                    {:fill-opacity alpha
                     :fill-image {:id id
                                  :width width
                                  :height height
                                  :mtype mtype
                                  ;; FIXME: we are not encodign the name, looks useless
                                  :name "sample"}}))]

    (if refs?
      (let [ref-file (buf/read-uuid mbuffer (+ moffset 4))
            ref-id   (buf/read-uuid mbuffer (+ moffset 20))]
        (-> fill
            (assoc :fill-color-ref-id ref-id)
            (assoc :fill-color-ref-file ref-file)))
      fill)))

#?(:clj
   (deftype Fills [size buffer ^:unsynchronized-mutable hash])

   :cljs
   #_:clj-kondo/ignore
   (deftype Fills [size dbuffer mbuffer cache ^:mutable __hash]
     Object
     (toString [_]
       "TODO")

     ;; cljs.core/ISequential
     ;; cljs.core/IEquiv
     ;; (-equiv [this other]
     ;;   (if (instance? Fills other)
     ;;     (let [obuffer (.-buffer ^js/DataView (.-buffer ^Fills other))
     ;;           cbuffer (.-buffer ^js/DataView buffer)]
     ;;       (if (= (.-byteLength obuffer)
     ;;              (.-byteLength cbuffer))
     ;;         (let [cb (js/Uint32Array. cbuffer)
     ;;               ob (js/Uint32Array. obuffer)
     ;;               sz (alength cb)]
     ;;           (loop [i 0]
     ;;             (if (< i sz)
     ;;               (if (== (aget ob i)
     ;;                       (aget cb i))
     ;;                 (recur (inc i))
     ;;                 false)
     ;;               true)))
     ;;         false))
     ;;     false))

     cljs.core/IReduce
     (-reduce [_ f]
       (loop [index  1
              result (if (pos? size)
                       (read-fill dbuffer mbuffer 0)
                       nil)]
         (if (< index size)
           (let [result (f result (read-fill dbuffer mbuffer index))]
             (if (reduced? result)
               @result
               (recur (inc index) result)))
           result)))

     (-reduce [_ f start]
       (loop [index  0
              result start]
         (if (< index size)
           (let [result (f result (read-fill dbuffer mbuffer index))]
             (if (reduced? result)
               @result
               (recur (inc index) result)))
           result)))

     cljs.core/IHash
     (-hash [coll]
       (caching-hash coll hash-ordered-coll __hash))

     cljs.core/ICounted
     (-count [_] size)

     cljs.core/IIndexed
     (-nth [_ i]
       (if (in-range? size i)
         (read-fill dbuffer mbuffer i)
         nil))

     (-nth [_ i default]
       (if (in-range? i size)
         (read-fill dbuffer mbuffer i)
         default))

     cljs.core/ISeqable
     (-seq [this]
       (when (pos? size)
         ((fn next-seq [i]
            (when (< i size)
              (cons (read-fill dbuffer mbuffer i)
                    (lazy-seq (next-seq (inc i))))))
          0)))))

(defn from-plain
  [fills]
  ;: TODO: add limits
  (let [fills   (into [] xf:take-fills fills)
        total   (count fills)
        dbuffer (buf/allocate (+ 4 (* MAX-FILLS FILL-BYTE-SIZE)))
        mbuffer (buf/allocate (* total METADATA-BYTE-SIZE))]

    (buf/write-byte dbuffer 0 total)

    (loop [index 0]
      (when (< index total)
        (let [fill     (nth fills index)
              doffset  (+ 4 (* index FILL-BYTE-SIZE))
              moffset  (* index METADATA-BYTE-SIZE)
              color    (get fill :fill-color)
              opacity  (get fill :fill-opacity 1)
              gradient (get fill :fill-color-gradient)
              image    (get fill :fill-image)]
          (cond
            (some? color)
            (do
              (write-solid-fill doffset dbuffer color opacity)
              (write-metadata moffset mbuffer fill)
              (recur (inc index)))

            (some? gradient)
            (do
              (write-gradient-fill doffset dbuffer gradient opacity)
              (write-metadata moffset mbuffer fill)
              (recur (inc index)))

            (some? image)
            (do
              (write-image-fill doffset dbuffer opacity image)
              (write-metadata moffset mbuffer fill)
              (recur (inc index)))

            :else
            (recur (inc index))))))

    #?(:cljs (Fills. total dbuffer mbuffer (weak-map/create) nil)
       :clj  (Fills. total dbuffer nil))))

(defn fills?
  [o]
  (instance? Fills o))




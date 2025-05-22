;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.fills
  (:require
   [app.common.math :as mth]
   [app.common.buffer :as buf]))

;; FIXME: reivisit
(def ^:const MAX-GRADIENT-STOPS 12)

(def ^:const GRADIENT-STOP-SIZE 8)
(def ^:const GRADIENT-BYTE-SIZE 156)
(def ^:const SOLID-BYTE-SIZE 4)
(def ^:const IMAGE-BYTE-SIZE 28)
(def ^:const FILL-BYTE-SIZE
  (+ 4 (max GRADIENT-BYTE-SIZE
            IMAGE-BYTE-SIZE
            SOLID-BYTE-SIZE)))

(defn- in-range?
  [size i]
  (and (< i size) (>= i 0)))

(defn- hex->rgb
  "Encode an hex string as rgb (int32)"
  [hex]
  #?(:clj (Integer/parseInt hex 16)
     :cljs (js/parseInt hex 16)))

(defn- rgb->rgba
  "Use the first 2 bytes of in32 for encode the alpha channel"
  [n alpha]
  (-> (mth/floor (* alpha 0xff))
      (unchecked-int)
      (bit-shift-left 24)
      (bit-or n)))

(defn- get-color-hex
  [n]
  (let [n (bit-and n 0x00ffffff)]
    (String/format "%x" (into-array Object [n]))))

(defn get-color-alpha
  [n]
  (let [n (bit-shift-right n 24)]
    (/ (float n) 0xff)))

(defn- write-solid-fill
  [offset buffer color alpha]
  (buf/write-byte buffer offset 0x00)
  (buf/write-int buffer (+ offset 4)
                 (-> (hex->rgb color)
                     (rgb->rgba alpha)))
  (+ offset FILL-BYTE-SIZE))

(def xf:take-stops
  (take MAX-GRADIENT-STOPS))

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
          (buf/write-float buffer (+ offset' 4) (:stop-offset stop))
          (recur (rest stops)
                 (+ offset' GRADIENT-STOP-SIZE)))
        (+ offset FILL-BYTE-SIZE)))))

(defn- write-image-fill
  [offset buffer id opacity image]
  (let [image-id (get image :id)
        image-width (get image :width)
        image-height (get image :height)]
    (buf/write-byte  buffer (+ offset  0) 0x03)
    (buf/write-uuid  buffer (+ offset  4) image-id)
    (buf/write-float buffer (+ offset 20) opacity)
    (buf/write-int   buffer (+ offset 24) image-width)
    (buf/write-int   buffer (+ offset 28) image-height)
    (+ offset FILL-BYTE-SIZE)))

(defn- read-stop
  [buffer offset]
  (let [rgba (buf/read-int buffer (+ offset 0))
        soff (buf/read-float buffer (+ offset 4))]
    {:color (get-color-hex rgba)
     :opacity (get-color-alpha rgba)
     :stop-offset soff}))

(defn- read-fill
  "Read segment from binary buffer at specified index"
  [buffer index]
  (let [offset (* index FILL-BYTE-SIZE)
        type   (long (buf/read-byte buffer offset))]
    (case type
      0
      (let [rgba  (buf/read-int buffer (+ offset 4))]
        {:fill-color (get-color-hex rgba)
         :fill-opacity (get-color-alpha rgba)})

      (1 2)
      (let [start-x (buf/read-float buffer (+ offset 4))
            start-y (buf/read-float buffer (+ offset 8))
            end-x   (buf/read-float buffer (+ offset 12))
            end-y   (buf/read-float buffer (+ offset 16))
            alpha   (buf/read-float buffer (+ offset 20))
            width   (buf/read-float buffer (+ offset 24))
            stops   (long (buf/read-byte buffer (+ offset 28)))
            type    (if (= type 1)
                      :linear
                      :radial)
            stops   (loop [index  0
                           result []]
                      (when (< index stops)
                        (recur (inc index)
                               (conj result (read-stop buffer (+ offset 32 (* GRADIENT-STOP-SIZE index)))))))]
        {:fill-opacity alpha
         :fill-color-gradient {:start-x start-x
                               :start-y start-y
                               :end-x end-x
                               :end-y end-y
                               :width width
                               :stops stops
                               :type type}})

      3
      (let [id     (buf/read-uuid buffer (+ offset 4))
            alpha  (buf/read-float buffer (+ offset 20))
            width  (buf/read-float buffer (+ offset 24))
            height (buf/read-float buffer (+ offset 28))]
        {:fill-opacity alpha
         :fill-image {:id id
                      :width width
                      :height height
                      ;; FIXME: missing on encoded data
                      :mtype "image/jpeg"}}))))

;; #?(:cljs
;;    #_:clj-kondo/ignore
;;    (deftype Fills [size buffer dview cache ^:mutable __hash]
;;      Object
;;      (toString [_]
;;        "TODO")

;;      cljs.core/ICounted
;;      (-count [_] size)

;;      cljs.core/IIndexed
;;      (-nth [_ i]
;;        (if (in-range? size i)
;;          (read-fill dview i)
;;          nil))

;;      (-nth [_ i default]
;;        (if (in-range? i size)
;;          (read-fill dview i)
;;          default))

;;      cljs.core/ISeqable
;;      (-seq [this]
;;        (when (pos? size)
;;          ((fn next-seq [i]
;;             (when (< i size)
;;               (cons (read-fill dview i)
;;                     (lazy-seq (next-seq (inc i))))))
;;           0)))))

;; (defn from-plain
;;   [fills]
;;   ;: TODO: add limits

;;   (let [total    (count fills)
;;         #?@(:cljs [buffer' (buf/allocate (* total FILL-BYTE-SIZE))
;;                    buffer  (new js/DataView buffer')]
;;             :clj  [buffer  (buf/allocate (* total FILL-BYTE-SIZE))])]
;;     (loop [index 0]
;;       (when (< index total)
;;         (let [fill     (nth fills index)
;;               offset   (* index FILL-BYTE-SIZE)
;;               color    (get fill :fill-color)
;;               opacity  (get fill :fill-opacity 1)
;;               gradient (get fill :fill-color-gradient)
;;               image    (get fill :fill-image)]
;;           (cond
;;             (some? color)
;;             (do
;;               (write-solid-fill offset buffer color opacity)
;;               (recur (inc index)))

;;             (some? gradient)
;;             (do
;;               (write-gradient-fill offset buffer gradient opacity)
;;               (recur (inc index)))

;;             (some? image)
;;             (do
;;               (write-image-fill offset buffer opacity image)
;;               (recur (inc index)))

;;             :else
;;             (recur (inc index))))))))




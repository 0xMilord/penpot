;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.types.shape-path-data-test
  (:require
   #?(:clj [app.common.fressian :as fres])
   [app.common.data :as d]
   [app.common.geom.point :as gpt]
   [app.common.math :as mth]
   [app.common.pprint :as pp]
   [app.common.transit :as trans]
   [app.common.types.path :as path]
   [app.common.types.path.segment :as path.segment]
   [clojure.test :as t]))

(def sample-content
  [{:command :move-to, :params {:x 480.0, :y 839.0}}
   {:command :line-to, :params {:x 439.0, :y 802.0}}
   {:command :curve-to, :params {:c1x 368.0, :c1y 737.0, :c2x 310.0, :c2y 681.0, :x 264.0, :y 634.0}}
   {:command :close-path :params {}}])

(def sample-bytes
  [0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 67 -16 0 0 68 81 -64 0
   0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 67 -37 -128 0 68 72 -128 0
   0 3 0 0 67 -72 0 0 68 56 64 0 67 -101 0 0 68 42 64 0 67 -124 0 0 68 30 -128 0
   0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])

;; ;; This means it implements IReduceInit/IReduce protocols
;; (t/deftest path-data-to-vector
;;   (let [pdata  (path/path-data sample-content)
;;         result (vec pdata)]
;;     (t/is (= 4 (count result)))
;;     (t/is (= (get-in sample-content [0 :command])
;;              (get-in result [0 :command])))
;;     (t/is (= (get-in sample-content [1 :command])
;;              (get-in result [1 :command])))
;;     (t/is (= (get-in sample-content [2 :command])
;;              (get-in result [2 :command])))
;;     (t/is (= (get-in sample-content [3 :command])
;;              (get-in result [3 :command])))

;;     (t/is (= (get-in sample-content [0 :params])
;;              (get-in result [0 :params])))
;;     (t/is (= (get-in sample-content [1 :params])
;;              (get-in result [1 :params])))
;;     (t/is (= (get-in sample-content [2 :params])
;;              (get-in result [2 :params])))
;;     (t/is (= (get-in sample-content [3 :params])
;;              (get-in result [3 :params])))))

;; (t/deftest path-data-plain-to-binary
;;   (let [pdata (path/path-data sample-content)]
;;     (t/is (= sample-bytes
;;              (vec
;;               #?(:cljs (js/Int8Array. (.-buffer pdata))
;;                  :clj  (.array (.-buffer pdata))))))
;;     (t/is (= sample-content
;;              (vec pdata)))))

;; (t/deftest path-data-transit-roundtrip
;;   (let [pdata (path/path-data sample-content)
;;         result1 (trans/encode-str pdata)
;;         expected "[\"~#penpot/path-data\",\"~bAAEAAAAAAAAAAAAAAAAAAAAAAABD8AAARFHAAAACAAAAAAAAAAAAAAAAAAAAAAAAQ9uAAERIgAAAAwAAQ7gAAEQ4QABDmwAARCpAAEOEAABEHoAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==\"]"
;;         result2 (trans/decode-str result1)]

;;     (t/is (= expected result1))
;;     (t/is (= pdata result2))))

;; #?(:clj
;;    (t/deftest path-data-fresian
;;      (let [pdata (path/path-data sample-content)
;;            result1 (fres/encode pdata)
;;            result2 (fres/decode result1)]
;;        (t/is (= pdata result2)))))


(def sample-content-large
  [{:command :move-to, :params {:x 480.0, :y 839.0}}
   {:command :line-to, :params {:x 439.0, :y 802.0}}
   {:command :curve-to, :params {:c1x 368.488, :c1y 737.253, :c2x 310.193, :c2y 681.399, :x 264.116, :y 634.439}}
   {:command :curve-to, :params {:c1x 218.039, :c1y 587.48, :c2x 181.333, :c2y 545.5, :x 154.0, :y 508.5}}
   {:command :curve-to, :params {:c1x 126.667, :c1y 471.5, :c2x 107.5, :c2y 438.0, :x 96.5, :y 408.0}}
   {:command :curve-to, :params {:c1x 85.5, :c1y 378.0, :c2x 80.0, :c2y 347.667, :x 80.0, :y 317.0}}
   {:command :curve-to, :params {:c1x 80.0, :c1y 256.897, :c2x 100.167, :c2y 206.704, :x 140.5, :y 166.423}}
   {:command :curve-to, :params {:c1x 180.833, :c1y 126.141, :c2x 230.667, :c2y 106.0, :x 290.0, :y 106.0}}
   {:command :curve-to, :params {:c1x 328.0, :c1y 106.0, :c2x 363.16700000000003, :c2y 115.0, :x 395.5, :y 133.0}}
   {:command :curve-to, :params {:c1x 427.83299999999997, :c1y 151.0, :c2x 456.0, :c2y 177.0, :x 480.0, :y 211.0}}
   {:command :curve-to, :params {:c1x 508.0, :c1y 175.0, :c2x 537.667, :c2y 148.5, :x 569.0, :y 131.5}}
   {:command :curve-to, :params {:c1x 600.333, :c1y 114.5, :c2x 634.0, :c2y 106.0, :x 670.0, :y 106.0}}
   {:command :curve-to, :params {:c1x 729.333, :c1y 106.0, :c2x 779.167, :c2y 126.14099999999999, :x 819.5, :y 166.423}}
   {:command :curve-to, :params {:c1x 859.833, :c1y 206.704, :c2x 880.0, :c2y 256.897, :x 880.0, :y 317.0}}
   {:command :curve-to, :params {:c1x 880.0, :c1y 347.66700000000003, :c2x 874.5, :c2y 378.0, :x 863.5, :y 408.0}}
   {:command :curve-to, :params {:c1x 852.5, :c1y 438.0, :c2x 833.333, :c2y 471.5, :x 806.0, :y 508.5}}
   {:command :curve-to, :params {:c1x 778.667, :c1y 545.5, :c2x 741.961, :c2y 587.48, :x 695.884, :y 634.439}}
   {:command :curve-to, :params {:c1x 649.807, :c1y 681.399, :c2x 591.512, :c2y 737.253, :x 521.0, :y 802.0}}
   {:command :line-to, :params {:x 480.0, :y 839.0}}
   {:command :close-path, :params {}}
   {:command :move-to, :params {:x 480.0, :y 760.0}}
   {:command :curve-to, :params {:c1x 547.491, :c1y 698.003, :c2x 603.03, :c2y 644.837, :x 646.6179999999999, :y 600.502}}
   {:command :curve-to, :params {:c1x 690.206, :c1y 556.167, :c2x 724.833, :c2y 517.333, :x 750.5, :y 484.0}}
   {:command :curve-to, :params {:c1x 776.167, :c1y 450.66700000000003, :c2x 794.167, :c2y 420.955, :x 804.5, :y 394.865}}
   {:command :curve-to, :params {:c1x 814.833, :c1y 368.774, :c2x 820.0, :c2y 342.868, :x 820.0, :y 317.145}}
   {:command :curve-to, :params {:c1x 820.0, :c1y 273.048, :c2x 806.0, :c2y 236.83299999999997, :x 778.0, :y 208.5}}
   {:command :curve-to, :params {:c1x 750.0, :c1y 180.16700000000003, :c2x 714.075, :c2y 166.0, :x 670.225, :y 166.0}}
   {:command :curve-to, :params {:c1x 635.876, :c1y 166.0, :c2x 604.0840000000001, :c2y 176.5, :x 574.85, :y 197.5}}
   {:command :curve-to, :params {:c1x 545.617, :c1y 218.5, :c2x 522.0, :c2y 248.0, :x 504.0, :y 286.0}}
   {:command :line-to, :params {:x 455.0, :y 286.0}}
   {:command :curve-to, :params {:c1x 437.66700000000003, :c1y 248.667, :c2x 414.383, :c2y 219.333, :x 385.15, :y 198.0}}
   {:command :curve-to, :params {:c1x 355.916, :c1y 176.667, :c2x 324.12399999999997, :c2y 166.0, :x 289.775, :y 166.0}}
   {:command :curve-to, :params {:c1x 245.925, :c1y 166.0, :c2x 210.0, :c2y 180.167, :x 182.0, :y 208.5}}
   {:command :curve-to, :params {:c1x 154.0, :c1y 236.833, :c2x 140.0, :c2y 273.105, :x 140.0, :y 317.31600000000003}}
   {:command :curve-to, :params {:c1x 140.0, :c1y 343.105, :c2x 145.167, :c2y 369.16700000000003, :x 155.5, :y 395.5}}
   {:command :curve-to,
    :params {:c1x 165.833, :c1y 421.83299999999997, :c2x 183.833, :c2y 451.83299999999997, :x 209.5, :y 485.5}}
   {:command :curve-to, :params {:c1x 235.167, :c1y 519.167, :c2x 270.0, :c2y 558.0, :x 314.0, :y 602.0}}
   {:command :curve-to, :params {:c1x 358.0, :c1y 646.0, :c2x 413.33299999999997, :c2y 698.667, :x 480.0, :y 760.0}}
   {:command :close-path, :params {}}
   {:command :move-to, :params {:x 480.0, :y 463.0}}
   {:command :close-path, :params {}}])

;; (t/deftest extremities-1
;;   (let [result (path.segment/calculate-extremities sample-content)]
;;     (t/is (= result
;;              #{(gpt/point 480 839)
;;                (gpt/point 439 802)
;;                (gpt/point 264 634)}))))

;; (t/deftest extremities-2
;;   (let [result (path.segment/calculate-extremities-2 sample-content)]
;;     (t/is (= result
;;              #{(gpt/point 480 839)
;;                (gpt/point 439 802)
;;                (gpt/point 264 634)}))))

;; (t/deftest extremities-3
;;   (let [result1 (path.segment/calculate-extremities-2 sample-content-large)
;;         result2 (path.segment/calculate-extremities sample-content-large)]
;;     ;; (prn result2)
;;     (t/is (= result1 result2))))


(def sample-content-2
  [{:command :move-to, :params {:x 480.0, :y 839.0}}
   {:command :line-to, :params {:x 439.0, :y 802.0}}
   {:command :curve-to, :params {:c1x 368.0, :c1y 737.0, :c2x 310.0, :c2y 681.0, :x 4.0, :y 4.0}}
   {:command :curve-to, :params {:c1x 3.0, :c1y 7.0, :c2x 30.0, :c2y -68.0, :x 20.0, :y 20.0}}
   {:command :close-path :params {}}])

(t/deftest extremities-4
  (let [result1 (path.segment/calculate-extremities-2 sample-content-2)
        result2 (path.segment/calculate-extremities sample-content-2)]
    ;; (prn result2)
    (t/is (= result1 result2))))


;; #?(:cljs
;;    (t/deftest extremities-bench
;;      (simple-benchmark [data sample-content-large]
;;        (path.segment/calculate-extremities sample-content-2)
;;        400000)

;;      (simple-benchmark [data sample-content-large]
;;        (path.segment/calculate-extremities-2 sample-content-2)
;;        400000)))



;; #?(:cljs
;;    (t/deftest extremities-bench
;;      (simple-benchmark [data sample-content-large]
;;        (path.segment/calculate-extremities sample-content-large)
;;        10000)

;;      (simple-benchmark [data sample-content-large]
;;        (path.segment/calculate-extremities-2 sample-content-large)
;;        10000)))


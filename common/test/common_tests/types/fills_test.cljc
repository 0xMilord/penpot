;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns common-tests.types.fills-test
  (:require
   #?(:clj [app.common.fressian :as fres])
   [app.common.data :as d]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.math :as mth]
   [app.common.pprint :as pp]
   [app.common.transit :as trans]
   [app.common.types.fills :as types.fills]
   [clojure.test :as t]))

(def sample-fills
  [{:fill-color "#fabada"
    :fill-opacity 0.7}])

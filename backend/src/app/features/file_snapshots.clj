;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.features.file-snapshots
  (:require
   [app.common.data :as d]
   [app.util.time :as dt]
   [app.common.exceptions :as ex]
   [app.features.fdata :as feat.fdata]
   [app.db :as db]
   [app.db.sql :as-alias sql]))

(def sql:snapshots
  "SELECT c.id,
          c.label,
          c.revn,
          c.created_at,
          c.updated_at AS modified_at,
          c.deleted_at,
          c.profile_id,
          c.created_by,
          c.revn,
          c.features,
          c.migrations,
          c.version,
          c.file_id,
          c.data AS legacy_data,
          fd.data AS data
     FROM file_change AS c
     LEFT JOIN file_data AS fd ON (fd.file_id = c.file_id
                                   AND fd.id = c.id
                                   AND fd.type = 'snapshot')
    WHERE c.label IS NOT NULL")

(def ^:private sql:get-snapshot
  (str sql:snapshots " AND c.file_id = ? AND c.id = ?"))

(def ^:private sql:get-snapshots
  (str sql:snapshots " AND c.file_id = ?"))

(defn get-snapshot
  [cfg file-id snapshot-id]
  (some->> (db/get-with-sql cfg [sql:get-snapshot file-id snapshot-id])
           (feat.fdata/resolve-file-data cfg)
           (feat.fdata/decode-file-data cfg)))

(defn update-snapshot!
  [cfg {:keys [id file-id label modified-at]}]
  (let [modified-at (or modified-at (dt/now))
        conn        (db/get-connection cfg)]
    (-> (db/update! conn :file-change
                    {:label label
                     :created-by "user"
                     :updated-at modified-at
                     :deleted-at nil}
                    {:file-id file-id
                     :id id}
                    {::db/return-keys false})
        (db/get-update-count)
        (pos?))))

(defn reduce-snapshots
  "Process the file snapshots using efficient reduction."
  [cfg file-id xform f init]
  (let [conn  (db/get-connection cfg)
        xform (comp
               (map (partial feat.fdata/resolve-file-data cfg))
               (map (partial feat.fdata/decode-file-data cfg))
               xform)]

    (->> (db/plan conn [sql:get-snapshots file-id] {:fetch-size 1})
         (transduce xform f #{}))))


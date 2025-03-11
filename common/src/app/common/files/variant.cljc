(ns app.common.files.variant
  (:require
   [app.common.data.macros :as dm]
   [app.common.types.components-list :as ctcl]
   [cuerdas.core :as str]))


(defn find-related-components
  "Find a list of the components thet belongs to this variant-id"
  [data objects variant-id]
  (->> (dm/get-in objects [variant-id :shapes])
       (map #(dm/get-in objects [% :component-id]))
       (map #(ctcl/get-component data % true))
       reverse))


(defn- dashes-to-end
  [data]
  (let [dashes (if (some #(= % "--") data) ["--"] [])]
    (concat (remove #(= % "--") data) dashes)))


(defn extract-properties-values
  [data objects variant-id]
  (->> (find-related-components data objects variant-id)
       (mapcat :variant-properties)
       (group-by :name)
       (map (fn [[k v]]
              {:name k
               :values (->> v
                            (map #(if (str/empty? (:value %)) "--" (:value %)))
                            distinct
                            dashes-to-end)}))))

(defn get-variant-mains
  [component data]
  (when-let [variant-id (:variant-id component)]
    (let [page-id (:main-instance-page component)
          objects (-> (dm/get-in data [:pages-index page-id])
                      (get :objects))]
      (dm/get-in objects [variant-id :shapes]))))


(defn is-secondary-variant?
  [component data]
  (let [shapes  (get-variant-mains component data)]
    (and (seq shapes)
         (not= (:main-instance-id component) (last shapes)))))


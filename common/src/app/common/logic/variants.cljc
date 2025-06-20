(ns app.common.logic.variants
  (:require
   [app.common.data :as d]
   [app.common.files.changes-builder :as pcb]
   [app.common.files.helpers :as cfh]
   [app.common.files.variant :as cfv]
   [app.common.logic.libraries :as cll]
   [app.common.logic.shapes :as cls]
   [app.common.logic.variant-properties :as clvp]
   [app.common.types.component :as ctk]
   [app.common.types.container :as ctn]
   [app.common.types.file :as ctf]
   [app.common.types.variant :as ctv]
   [app.common.uuid :as uuid]))

(defn generate-add-new-variant
  [changes shape variant-id new-component-id new-shape-id prop-num]
  (let [data                (pcb/get-library-data changes)
        objects             (pcb/get-objects changes)
        component-id        (:component-id shape)
        value               (str ctv/value-prefix
                                 (-> (cfv/extract-properties-values data objects variant-id)
                                     last
                                     :value
                                     count
                                     inc))

        [new-shape changes] (-> changes
                                (cll/generate-duplicate-component
                                 {:data data}
                                 component-id
                                 new-component-id
                                 {:new-shape-id new-shape-id :apply-changes-local-library? true}))]
    (-> changes
        (clvp/generate-update-property-value new-component-id prop-num value)
        (pcb/change-parent (:parent-id shape) [new-shape] 0))))

(defn- generate-path
  [path objects base-id shape]
  (let [get-type #(case %
                    :frame :container
                    :group :container
                    :rect  :shape
                    :circle :shape
                    :bool :shape
                    :path :shape
                    %)]
    (if (= base-id (:id shape))
      path
      (generate-path (str path " " (:name shape) (get-type (:type shape))) objects base-id (get objects (:parent-id shape))))))

(defn- add-unique-path
  "Adds a new property :shape-path to the shape, with the path of the shape.
   Suffixes like -1, -2, etc. are added to ensure uniqueness."
  [shapes objects base-id]
  (letfn [(unique-path [shape counts]
            (let [path (generate-path "" objects base-id shape)
                  num  (get counts path 1)]
              [(str path "-" num) (update counts path (fnil inc 1))]))]
    (first
     (reduce
      (fn [[result counts] shape]
        (let [[shape-path counts'] (unique-path shape counts)]
          [(conj result (assoc shape :shape-path shape-path)) counts']))
      [[] {}]
      shapes))))

(defn- keep-swapped-item
  [changes current-shape previous-shape previous-shapes ldata page libraries orig-ref-shape]
  (let [objects (pcb/get-objects changes)

        previous-shapes-map       (into {} (map (juxt :id identity) previous-shapes))

        current-shape-ref-shape (ctf/find-ref-shape nil {:objects objects} libraries current-shape)

        swap-slot (ctk/get-swap-slot previous-shape)
        swap-ref (ctk/get-swap-ref previous-shape)
        update-swap-slot? (contains? previous-shapes-map swap-slot)

        _ (prn "orig-ref-shape" (:id orig-ref-shape))
        _ (prn "previous-shapes-map" (keys previous-shapes-map))
        _ (prn "current-shape-ref-shape" (:id current-shape-ref-shape))
        _ (prn "swap-slot" swap-slot)
        _ (prn "swap-ref" swap-ref)
        _ (prn "update-swap-slot?" update-swap-slot?)



        prev-shapes-ids (->> (cfh/get-children-with-self previous-shapes-map (:id previous-shape))
                             (map :id)
                             set)

        ;; TODO This is waaaaay too dirty
        fake-changes (-> changes
                         (assoc :redo-changes [])
                         (pcb/change-parent uuid/zero [previous-shape] 0 {:component-swap true})
                         (pcb/update-shapes
                          [(:id previous-shape)]
                          (fn [shape]
                            (cond-> shape
                              (= swap-slot swap-ref)
                              (ctk/set-swap-slot (:id current-shape-ref-shape))
                              (not= swap-slot swap-ref)
                              (ctk/set-swap-slot swap-slot)
                              :always
                              (ctk/set-swap-ref (:id current-shape-ref-shape))))))


        ;; remove the delete changes for the swapped item and its children
        ;; TODO: Maybe it makes sense to change the deletion process to don't add them
        ;; on the first place
        redo   (remove #(and (= (:type %) :del-obj) (contains? prev-shapes-ids (:id %))) (:redo-changes changes))
        changes (assoc changes :redo-changes redo)

        ;; TODO undo-changes?
        changes (-> (assoc changes :redo-changes (into (:redo-changes fake-changes) redo))
                    #_(pcb/concat-changes fake-changes changes)
                    ;; TODO Keep pos
                    (pcb/change-parent (:parent-id current-shape) [previous-shape] 0 {:component-swap true}))]

      ;; Delete new non-swapped item
    (-> changes
        (cls/generate-delete-shapes ldata page objects (d/ordered-set (:id current-shape)) {:component-swap true})
        second)))


(defn generate-keep-touched
  [changes new-shape original-shape original-shapes page libraries ldata]
  (let [objects            (pcb/get-objects changes)
        container          (ctn/make-container page :page)

        orig-touched       (filter (comp seq :touched) original-shapes)

        new-shapes-w-path  (add-unique-path
                            (reverse (cfh/get-children-with-self objects (:id new-shape)))
                            objects
                            (:id new-shape))
        new-shapes-map     (into {} (map (juxt :shape-path identity) new-shapes-w-path))

        orig-ref-shape     (ctf/find-ref-shape nil container libraries original-shape)
        o-ref-shapes-wp    (add-unique-path
                            (reverse (cfh/get-children-with-self objects (:id orig-ref-shape)))
                            objects
                            (:id orig-ref-shape))
        o-ref-shapes-p-map (into {} (map (juxt :id :shape-path) o-ref-shapes-wp))]
    (reduce
     (fn [changes previous-shape]
       (let [;; TODO use `ctf/find-swap-slot` ?
             swap-slot      (ctk/get-swap-slot previous-shape)
             swap-ref       (ctk/get-swap-ref previous-shape)
             prev-shape-ref (ctf/find-ref-shape nil container libraries previous-shape)
             shape-path     (or (get o-ref-shapes-p-map (:id prev-shape-ref))
                                (get o-ref-shapes-p-map swap-slot)
                                (get o-ref-shapes-p-map swap-ref))

             current-shape  (get new-shapes-map shape-path)




             _ (prn "prev-shape-ref" (:id prev-shape-ref))
             _ (prn "shape-path" shape-path)
             _ (prn "current-shape" (:id current-shape))

             ss (ctf/find-swap-slot previous-shape container nil libraries)
             _ (prn "swap-slot" swap-slot)
             _ (prn "ss" ss)]
         ;; TODO Ignore children of swapped items
         (if current-shape
           (if swap-slot
             (keep-swapped-item changes current-shape previous-shape original-shapes ldata page libraries orig-ref-shape)
             (cll/update-attrs-on-switch
              changes current-shape previous-shape new-shape original-shape prev-shape-ref container))
           changes)))
     changes
     orig-touched)))


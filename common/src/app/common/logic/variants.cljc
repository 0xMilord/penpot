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
  [changes current-shape prev-shape prev-objects ldata page swap-ref-id]
  (let [objects         (pcb/get-objects changes)
        prev-swap-slot  (ctk/get-swap-slot prev-shape)

        ;; TODO This is waaaaay too dirty
        fake-changes (cond-> changes
                       :always
                       (assoc :redo-changes [])

                       :always
                       ;; Temporary move the previous shape to the root panel
                       (pcb/change-parent uuid/zero [prev-shape] 0 {:component-swap true})

                       (= prev-swap-slot swap-ref-id)
                       ;; We need to update the swap slot only when it pointed
                       ;; to the swap-ref-id
                       (pcb/update-shapes
                        [(:id prev-shape)]
                        #(ctk/set-swap-slot % (:shape-ref current-shape))))

        remove-deletion-ids (->> (cfh/get-children-with-self prev-objects (:id prev-shape))
                                 (map :id)
                                 set)

        ;; remove the delete changes for the swapped item and its children
        ;; TODO: Maybe it makes sense to change the deletion process to don't add them
        ;; on the first place
        redo   (remove #(and (= (:type %) :del-obj) (contains? remove-deletion-ids (:id %))) (:redo-changes changes))
        changes (assoc changes :redo-changes redo)

        ;; TODO undo-changes?
        changes (-> (assoc changes :redo-changes (into (:redo-changes fake-changes) redo))
                    #_(pcb/concat-changes fake-changes changes)
                    ;; TODO Keep pos
                    (pcb/change-parent (:parent-id current-shape) [prev-shape] 0 {:component-swap true}))]

      ;; Delete new non-swapped item
    (-> changes
        (cls/generate-delete-shapes ldata page objects (d/ordered-set (:id current-shape)) {:component-swap true})
        second)))


(defn- child-of-swapped?
  "Check if any ancestor of a shape (until reach a base-parent-id) was swapped"
  [shape objects base-parent-id]
  (->> (ctn/get-parent-heads objects shape)
       ;; Ignore ancestors ahead of base-parent
       (drop-while #(not= base-parent-id (:id %)))
       (vec)
       ;; Ignore first and last (base-parent and shape)
       (pop)
       (rest)
       (some ctk/get-swap-slot)))

(defn generate-keep-touched
  "On a switch operation, copy the touched attributes from the original shape
   and its children to the new shape (and its children).
   The match between shapes are defined by their names on the components and
   their shape-path (a string formed by the types of its ancestors)"
  [changes new-shape original-shape original-shapes page libraries ldata]
  (let [objects            (pcb/get-objects changes)
        container          (ctn/make-container page :page)
        page-objects       (:objects page)

        orig-touched       (->> (filter (comp seq :touched) original-shapes)
                                ;; Ignore children of swapped items, because
                                ;; they will be moved without change when
                                ;; managing their swapped ancestor
                                (remove
                                 #(child-of-swapped? %
                                                     page-objects
                                                     (:id original-shape))))
        original-objects   (into {} (map (juxt :id identity) original-shapes))

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
       (let [swap-slot      (ctk/get-swap-slot previous-shape)

             ;; If there is no swap slot, get the referenced shape
             prev-shape-ref (when-not swap-slot
                              ;; TODO Maybe just get it from o-ref-shapes-wp
                              (ctf/find-ref-shape nil container libraries previous-shape))
             ;; If there is a swap slot, find the related shape id
             swap-ref-id    (when swap-slot
                              (ctf/find-ref-id-for-swapped previous-shape container libraries))


             shape-path     (or (get o-ref-shapes-p-map (:id prev-shape-ref))
                                (get o-ref-shapes-p-map swap-ref-id))

             current-shape  (get new-shapes-map shape-path)]


         (if current-shape
           (if swap-slot
             (keep-swapped-item changes current-shape previous-shape original-objects ldata page swap-ref-id)
             (cll/update-attrs-on-switch
              changes current-shape previous-shape new-shape original-shape prev-shape-ref container))
           changes)))
     changes
     orig-touched)))


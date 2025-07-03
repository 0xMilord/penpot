;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.ds.controls.numeric-input
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.schema :as sm]
   [app.common.uuid :as uuid]
   [app.main.constants :refer [max-input-length]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.select :refer [get-option handle-focus-change]]
   [app.main.ui.ds.controls.shared.options-dropdown :refer [options-dropdown*]]
   [app.main.ui.ds.controls.shared.token-option :refer [schema:token-option]]
   [app.main.ui.ds.controls.utilities.input-field :refer [input-field*]]
   [app.main.ui.ds.foundations.assets.icon :refer [icon* icon-list]]
   [app.main.ui.ds.tooltip :refer [tooltip*]]
   [app.main.ui.formats :as fmt]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.object :as obj]
   [app.util.simple-math :as smt]
   [cuerdas.core :as str]
   [goog.events :as events]
   [rumext.v2 :as mf]
   [rumext.v2.util :as mfu]))

(defn- clamp
  "Returns `min-val` if `val` is less than `min-val`, `max-val`
   if greater than `max-val`, or `val` itself if within bounds."
  [val min-val max-val]
  (-> val
      (max min-val)
      (min max-val)))

(defn- increment
  "Increments `val` by `step`, clamped to [`min-val`, `max-val`]."
  [val step min-val max-val]
  (clamp (+ val step) min-val max-val))

(defn- decrement
  "Decrements `val` by `step`, clamped to [`min-val`, `max-val`]."
  [val step min-val max-val]
  (clamp (- val step) min-val max-val))

(defn- parse-value
  "Parses and clamps `raw-value` as a number within bounds;
   returns nil if invalid or empty."
  [raw-value last-value min-value max-value nillable]
  (let [new-value (-> raw-value
                      (str/strip-suffix ".")
                      (smt/expr-eval last-value))]
    (cond
      (and nillable (nil? raw-value))
      nil

      (d/num? new-value)
      (-> new-value
          (d/max (/ sm/min-safe-int 2))
          (d/min (/ sm/max-safe-int 2))
          (cond-> (d/num? min-value)
            (d/max min-value))
          (cond-> (d/num? max-value)
            (d/min max-value)))

      :else nil)))
;; (defn get-option
;;   [options id]
;;   (or (d/seek #(= id (get % :id)) options)
;;       (nth options 0)))
(defn get-option-by-name
  [options name]
  (d/seek #(= name (get % :name)) options))

(def ^:private schema:token-field
  [:map
   [:id :string]
   [:label :string]
   [:value [:or 
            :int
            :string]]
   [:disabled {:optional true} :boolean]
   [:slot-start {:optional true} [:maybe some?]]
   [:on-click {:optional true} fn?]
   [:handle-pill fn?]
   [:on-blur {:optional true} fn?]
   [:detach-token fn?]])

(mf/defc token-field*
  {::mf/private true
   ::mf/schema schema:token-field}
  [{:keys [id label value slot-start disabled
           on-click handle-pill on-blur detach-token
           token-wrapper-ref token-detach-btn-ref
           ]}]

  (let [_ (prn "carga token-field")
        focus-wrapper
        (mf/use-fn
         (mf/deps token-wrapper-ref disabled)
         (fn [event]
           (when-not disabled
             (dom/prevent-default event)
             (dom/focus! (mf/ref-val token-wrapper-ref)))))]

    [:div {:class (stl/css-case :token-field true
                                :with-icon (some? slot-start)
                                :token-field-disabled disabled)
           :on-click focus-wrapper
           :disabled disabled
           :on-key-down handle-pill
           :ref token-wrapper-ref
           :on-blur on-blur
           :tab-index (if disabled -1 0)}

     (when (some? slot-start)
       slot-start)
     [:> tooltip* {:content label
                   :id (dm/str id "-pill")}
      [:button {:on-click on-click
                :class (stl/css-case :pill true
                                     :pill-disabled disabled)
                :disabled disabled
                :aria-labelledby (dm/str id "-pill")
                :on-key-down handle-pill}
       value]]

     (when-not disabled
       [:> icon-button* {:variant "action"
                         :class (stl/css :invisible-button)
                         :icon "broken-link"
                         ;; TODO: add translation
                         :ref token-detach-btn-ref
                         :aria-label "Detach token"
                         :on-click detach-token}])]))

;; TODO: Review schema props
(def ^:private schema:numeric-input
  [:map
   [:id {:optional true} :string]
   [:class {:optional true} :string]
  ;;  [:value {:optional true} [:or
  ;;                            :int
  ;;                            :string]]
   [:default {:optional true} [:maybe :string]]
   [:placeholder {:optional true} :string]
   [:icon {:optional true} [:maybe [:and :string [:fn #(contains? icon-list %)]]]]
   [:min {:optional true} [:maybe :int]]
   [:max {:optional true} [:maybe :int]]
   [:max-length {:optional true} :int]
   [:step {:optional true} [:maybe :int]]
   [:is-selected-on-focus {:optional true} :boolean]
   [:nillable {:optional true} :boolean]
   [:token-applied {:optional true} [:maybe :string]]
   [:empty-to-end {:optional true} :boolean]
   [:on-change {:optional true} fn?]
   [:on-blur {:optional true} fn?]
   [:on-focus {:optional true} fn?]])

(defn group-options [grouped-options]
  (->> grouped-options
       (map (fn [[group-key items]]
              (cons {:group true
                     :name  group-key
                     :id    (uuid/next)}
                    items)))
       (interpose [{:separator true}])
       (apply concat)))

(mf/defc numeric-input*
  {::mf/forward-ref true
   ::mf/schema schema:numeric-input}
  [{:keys [id class value default placeholder icon disabled
           min max max-length step
           is-selected-on-focus nillable
           options token-applied empty-to-end
           on-change on-blur on-focus] :rest props} ref]

  (let [;; NOTE: we use mfu/bean here for transparently handle
        ;; options provide as clojure data structures or javascript
        ;; plain objects and lists.
        options      (group-options options)
        options      (if (array? options)
                       (mfu/bean options)
                       options)


        ;; Borrar
        on-change (d/nilv on-change #(prn "on-change value" %))

        ;; Defautl props
        nillable        (d/nilv nillable false)
        disabled        (d/nilv disabled false)
        select-on-focus (d/nilv is-selected-on-focus true)
        default         (d/parse-double default (when-not nillable 0))
        step            (d/parse-double step 1)
        min             (d/parse-double min sm/min-safe-int)
        max             (d/parse-double max sm/max-safe-int)
        max-length      (d/nilv max-length max-input-length)
        empty-to-end    (d/nilv empty-to-end false)
        internal-id     (mf/use-id)
        id              (or id internal-id)
        listbox-id      (mf/use-id)

        ;; State and values
        is-open*           (mf/use-state false)
        is-open            (deref is-open*)

        is-token*          (mf/use-state (some? token-applied))
        is-token           (deref is-token*)
        _ (prn "is-token" is-token)

        selected-token-id  (if token-applied
                             (:id (get-option-by-name options token-applied))
                             nil)
        selected-id*       (mf/use-state selected-token-id) ;; No viene una id, viene solo un nombre q es lo que tiene la shape
        selected-id        (deref selected-id*)
        _ (prn "selected-id" selected-id)

        focused-id*        (mf/use-state nil)
        focused-id         (deref focused-id*)

        ;; We are not managing filtering yet
        filter-id*         (mf/use-state "")
        filter-id          (deref filter-id*)

        is-multiple?       (= :multiple value)

        value        (cond
                       is-multiple? nil
                       (and nillable (nil? value)) nil
                       :else (d/parse-double value default))

        ;; Raw value is used to store the raw input value
        raw-value* (mf/use-ref
                    (cond
                      is-multiple?
                      ""

                      (and nillable (nil? value))
                      ""

                      :else
                      (fmt/format-number (d/parse-double value default))))

        ;; Last value is used to store the last valid value
        last-value* (mf/use-ref (d/parse-double value default))

        dropdown-options
        (mf/with-memo [options filter-id]
          (let [filter-id (str/trim (or filter-id ""))]
            (if (seq filter-id)
              (let [filtered (->> options
                                  (filterv (fn [option]
                                             (let [option-id (str/lower (get option :id ""))]
                                               (str/includes? option-id (str/lower filter-id)))))
                                  (not-empty))]
                (or filtered options))
              options)))

        ;; Refs
        wrapper-ref          (mf/use-ref nil)
        nodes-ref            (mf/use-ref nil)
        options-ref          (mf/use-ref nil)
        token-wrapper-ref    (mf/use-ref nil)
        internal-ref         (mf/use-ref nil)
        ref                  (or ref internal-ref)
        dirty-ref            (mf/use-ref false)
        open-dropdown-ref    (mf/use-ref nil)
        token-detach-btn-ref (mf/use-ref nil)

        set-option-ref
        (mf/use-fn
         (fn [node]
           (let [state (mf/ref-val nodes-ref)
                 state (d/nilv state #js {})
                 id    (dom/get-data node "id")
                 state (obj/set! state id node)]
             (mf/set-ref-val! nodes-ref state)
             (fn []
               (let [state (mf/ref-val nodes-ref)
                     state (d/nilv state #js {})
                     id    (dom/get-data node "id")
                     state (obj/unset! state id)]
                 (mf/set-ref-val! nodes-ref state))))))

        ;; Callbacks
        update-input
        (mf/use-fn
         (fn [new-value]
           (when-let [node (mf/ref-val ref)]
             (dom/set-value! node new-value))))

        apply-value
        (mf/use-fn
         (mf/deps on-change update-input value nillable)
         (fn [raw-value]
           (if-let [parsed (parse-value (str raw-value) (mf/ref-val last-value*) min max nillable)]
             (when-not (= parsed (mf/ref-val last-value*))
               (mf/set-ref-val! last-value* parsed)
               (reset! is-token* false)
               (when (fn? on-change)
                 (on-change parsed))

              ;; Comprar si es valor es necesario, sino borrar
               (mf/set-ref-val! raw-value* (fmt/format-number parsed))
               (update-input (fmt/format-number parsed)))

             (if (and nillable (empty? raw-value))
               (do
                 (mf/set-ref-val! last-value* nil)
                 (mf/set-ref-val! raw-value* "")
                 (reset! is-token* false)
                 (update-input "")
                 (when (fn? on-change)
                   (on-change nil)))

               (let [fallback-value (or (mf/ref-val last-value*) default)]
                 (mf/set-ref-val! raw-value* fallback-value)
                 (mf/set-ref-val!  last-value* fallback-value)
                 (reset! is-token* false)
                 (update-input (fmt/format-number fallback-value))
                 (when (and (fn? on-change) (not= fallback-value value))
                   (on-change fallback-value)))))))

        apply-token
        (fn [id])

        store-raw-value
        (mf/use-fn
         (fn [event]
           (let [text (dom/get-target-val event)]
             (mf/set-ref-val! raw-value* text))))

        on-blur
        (mf/use-fn
         (mf/deps apply-value)
         (fn [event]
           (let [target (dom/get-related-target event)
                 self-node (mf/ref-val wrapper-ref)]
             (when-not (dom/is-child? self-node target)
               (reset! focused-id* nil)
               (reset! is-open* false))

             (when (mf/ref-val dirty-ref)
               (apply-value (mf/ref-val raw-value*))
               (when (fn? on-blur)
                 (on-blur event))))))

        on-token-apply
        (fn [id value]
          (reset! selected-id* id)
          (reset! focused-id* nil)
          (reset! is-open* false)
          (apply-value value)
          ;; Hay que aplicar el token a la shape
          (apply-token id)
          (reset! is-token* id))

        on-option-click
        (mf/use-fn
         (mf/deps options)
         (fn [event]
           (let [node   (dom/get-current-target event)
                 id     (dom/get-data node "id")
                 option (get-option options (uuid/uuid id))
                 value  (get option :resolved-value)]

             (on-token-apply id value))))

        on-option-enter
        (mf/use-fn
         (mf/deps options focused-id apply-value)
         (fn [_]
           (let [option (get-option options (uuid/uuid focused-id))
                 value  (get option :resolved-value)]
             (on-token-apply id value))))

        on-blur
        (mf/use-fn
         (mf/deps apply-value on-blur)
         (fn [event]
           (let [target   (dom/get-related-target event)
                 self-node (mf/ref-val wrapper-ref)]
             (when-not (dom/is-child? self-node target)
               (reset! focused-id* nil)
               (reset! is-open* false)))
           (when (mf/ref-val dirty-ref)
             (apply-value (mf/ref-val raw-value*))
             (when (fn? on-blur)
               (on-blur event)))))

        handle-key-down
        (mf/use-fn
         (mf/deps apply-value update-input parse-value options)
         (fn [event]
           (mf/set-ref-val! dirty-ref true)
           (let [up?     (kbd/up-arrow? event)
                 down?   (kbd/down-arrow? event)
                 enter?  (kbd/enter? event)
                 esc?    (kbd/esc? event)
                 node    (mf/ref-val ref)
                 tokens? (= (.-key event) "{")
                 parsed  (parse-value (mf/ref-val raw-value*) (mf/ref-val last-value*) min max nillable)
                 current-value (or parsed default)
                 options (mf/ref-val options-ref)
                 len     (count options)
                 index   (d/index-of-pred options #(= focused-id (get % :id)))
                 index   (d/nilv index -1)]

             (cond
               (and (some? options) tokens?)
               (reset! is-open* true)

               enter?
               (if is-open
                 (on-option-enter event)
                 (on-blur event))

               esc?
               (do
                 (update-input (fmt/format-number (mf/ref-val last-value*)))
                 (reset! is-open* false)
                 (dom/blur! node))

               up?
               (if is-open
                 (let [new-index (if (= index -1)
                                   (dec len)
                                   (mod (- index 1) len))]
                   (handle-focus-change options focused-id* new-index nodes-ref))

                 (let [new-val (increment current-value step min max)]
                   (update-input (fmt/format-number new-val))
                   (apply-value (dm/str new-val))
                   (dom/prevent-default event)))

               down?
               (if is-open
                 (let [new-index (if (= index -1)
                                   0
                                   (mod (+ index 1) len))]
                   (handle-focus-change options focused-id* new-index nodes-ref))

                 (let [new-val (decrement current-value step min max)]
                   (update-input (fmt/format-number new-val))
                   (apply-value (dm/str new-val))
                   (dom/prevent-default event)))))))

        handle-focus
        (mf/use-fn
         (mf/deps on-focus select-on-focus)
         (fn [event]
           (when (fn? on-focus)
             (on-focus event))
           (let [target (dom/get-target event)]
             (when select-on-focus
               (dom/select-text! target)
               ;; In webkit browsers the mouseup event will be called after the on-focus causing and unselect
               (.addEventListener target "mouseup" dom/prevent-default #js {:once true})))))

        handle-mouse-wheel
        (mf/use-fn
         (mf/deps apply-value parse-value min max nillable)
         (fn [event]
           (when-let [node (mf/ref-val ref)]
             (when (dom/active? node)
               (let [inc? (->> (dom/get-delta-position event)
                               :y
                               (neg?))
                     parsed (parse-value (mf/ref-val raw-value*) (mf/ref-val last-value*) min max nillable)
                     current-value (or parsed default)
                     new-val (if inc?
                               (increment current-value step min max)
                               (decrement current-value step min max))]
                 (dom/prevent-default event)
                 (dom/stop-propagation event)
                 (apply-value (dm/str new-val)))))))

        open-dropdown
        (mf/use-fn
         (fn [event]
           (dom/prevent-default event)
           (reset! is-open* true)
           (dom/focus! (mf/ref-val ref))))

        open-dropdown-token
        (mf/use-fn
         (fn [event]
           (when-not disabled
             (dom/prevent-default event)
             (reset! is-open* true)
             (dom/focus! (mf/ref-val token-wrapper-ref)))))

        detach-token
        (mf/use-fn
         (mf/deps on-change)
         (fn [event]
           (when-not disabled
             (dom/prevent-default event)
             (dom/stop-propagation event)
             (reset! is-token* false)
             (reset! selected-id* nil)
             (reset! focused-id* nil)
             (dom/focus! (mf/ref-val ref)))))


        ;; Change this name for something more descriptive (on-tolken-key-down)
        handle-pill
        (mf/use-fn
         (mf/deps detach-token)
         (fn [event]
           (let [esc?    (kbd/esc? event)
                 delete? (kbd/delete? event)
                 backspace? (kbd/backspace? event)
                 enter? (kbd/enter? event)
                 up?     (kbd/up-arrow? event)
                 down?   (kbd/down-arrow? event)
                 options (mf/ref-val options-ref)
                 len     (count options)
                 index   (d/index-of-pred options #(= focused-id (get % :id)))
                 index   (d/nilv index -1)
                 detach-btn (mf/ref-val token-detach-btn-ref)
                 target (dom/get-target event)]

             (when-not disabled
               (cond
                 (or delete? backspace?)
                 (do
                   (dom/prevent-default event)
                   (detach-token event)
                   (dom/focus! (mf/ref-val ref)))

                 enter?
                 (if is-open
                   (on-option-enter event)
                   (when (not= target detach-btn)
                     (reset! is-open* true)))

                 esc?
                 (dom/blur! (mf/ref-val token-wrapper-ref))

                 up?
                 (when is-open
                   (let [new-index (if (= index -1)
                                     (dec len)
                                     (mod (- index 1) len))]
                     (handle-focus-change options focused-id* new-index nodes-ref)))

                 down?
                 (when is-open
                   (let [new-index (if (= index -1)
                                     0
                                     (mod (+ index 1) len))]
                     (handle-focus-change options focused-id* new-index nodes-ref))))))))

        props
        (mf/spread-props props {:ref ref
                                :type "text"
                                :id id
                                :placeholder (if is-multiple?
                                               (tr "settings.multiple")
                                               placeholder)
                                :default-value (or (mf/ref-val last-value*) (fmt/format-number value))
                                :on-blur on-blur
                                :on-key-down handle-key-down
                                :on-focus handle-focus
                                :on-change store-raw-value
                                :disabled disabled
                                :slot-start (when icon
                                              (mf/html [:> icon* {:icon-id icon
                                                                  :size "s"
                                                                  :class (stl/css :icon)}]))
                                :slot-end (when-not disabled
                                            (when (some? options)
                                              (mf/html [:> icon-button* {:variant "action"
                                                                         :icon "component"
                                                                         :class (stl/css :invisible-button)
                                                                         ;; TODO: add translation
                                                                         :aria-label "Open dropdown"
                                                                         :ref open-dropdown-ref
                                                                         :on-click open-dropdown}])))
                                :max-length max-length})

        token-props
        (when is-token
          (let [token (get-option options (uuid/uuid is-token))
                label (get token :name)
                token-value (get token :resolved-value)]
            (mf/spread-props props
                             {:id is-token
                              :label label
                              :value token-value
                              :on-click open-dropdown-token
                              :handle-pill handle-pill
                              :disabled disabled
                              :slot-start (when icon
                                            (mf/html [:> icon* {:icon-id icon
                                                                :class (stl/css :icon)}]))
                              :token-wrapper-ref token-wrapper-ref
                              :on-blur on-blur
                              :token-detach-btn-ref token-detach-btn-ref
                              :detach-token detach-token})))

        _ (prn "llega hasta la linea 607")
        _ (.log js/console (clj->js dropdown-options))]

    (mf/with-layout-effect [handle-mouse-wheel]
      (when-let [node (mf/ref-val ref)]
        (let [key (events/listen node "wheel" handle-mouse-wheel #js {:passive false})]
          #(events/unlistenByKey key))))

    (mf/with-effect [options]
      (mf/set-ref-val! options-ref options))

    [:div {:class (dm/str class " " (stl/css :input-wrapper))
           :ref wrapper-ref}
     (if is-token
       [:> token-field* token-props]

       [:> input-field* props])

     (when is-open
       [:> options-dropdown* {:on-click on-option-click
                              :id listbox-id
                              :options dropdown-options
                              :selected selected-id
                              :focused focused-id
                              :token-option true
                              :style {:width "247px"} ;; revisar
                              :empty-to-end empty-to-end
                              :ref set-option-ref}])]))
(ns kisekae.build
  "spec -> VrmDocument -> .vrm bytes, via kotoba-lang/org-vrmc-vrm's engine
   (ADR-2607071610). This namespace is the bridge between the editor's small
   EDN spec and the real VRM pipeline:

     docs-by-url (caller fetches)          kisekae.spec
            \\                                /
             `-> effective-sources <--------'      (pure selection)
                       |
                 vrm.compose/compose               (skeleton unification, engine)
                       |
              apply-material-edits, apply-meta     (pure doc transforms)
                       |
                 vrm.export/export-glb             (bytes, engine)

   Everything here is pure GIVEN the fetched documents — `docs-by-url` is
   `{url VrmDocument}`, resolved by the caller (browser `fetch` + `vrm.parse`
   in an app; a fixture map in tests). No I/O in this namespace, matching the
   engine's own zero-dep discipline and keeping the whole build path in the
   interop-free .cljc subset (runtime-priority note in kisekae.spec).

   Missing anything (an unfetched URL, a donor VRM with no part of the
   requested kind) THROWS ex-info — a build that silently substitutes parts
   would hand the user a character they didn't design."
  (:require [kisekae.spec :as spec]
            [vrm.part :as part]
            [vrm.compose :as compose]
            [vrm.export :as export]
            [vrm.vrm-types :as vt]))

(defn- doc-for [docs-by-url url]
  (or (get docs-by-url url)
      (throw (ex-info "kisekae: document not fetched" {:url url}))))

(defn- find-part [parts kind url]
  (or (first (filter #(= kind (:category %)) parts))
      (throw (ex-info "kisekae: donor VRM has no part of requested kind"
                      {:kind kind :url url :available (mapv :category parts)}))))

(defn effective-sources
  "spec + docs-by-url -> {:sources [{:part VrmPart :doc VrmDocument} ...]
                          :skeleton-base <index into :sources>}

   Dress-up resolution: the base avatar contributes every part EXCEPT the
   kinds the spec overrides; each override contributes exactly the matching
   part from its donor document. The base's :body is always present and is
   the skeleton compose unifies onto."
  [s docs-by-url]
  (let [base-url (get-in s [:spec/base :vrm/url])
        base-doc (doc-for docs-by-url base-url)
        base-parts (part/decompose base-doc)
        overridden (spec/overridden-kinds s)
        kept-base (vec (remove #(contains? overridden (:category %)) base-parts))
        _ (when-not (some #(= :body (:category %)) kept-base)
            (throw (ex-info "kisekae: base VRM has no :body part" {:url base-url})))
        base-sources (mapv (fn [p] {:part p :doc base-doc}) kept-base)
        donor-sources (mapv (fn [{:part/keys [kind source]}]
                              (let [url (:vrm/url source)
                                    d (doc-for docs-by-url url)]
                                {:part (find-part (part/decompose d) kind url) :doc d}))
                            (:spec/parts s))
        sources (into base-sources donor-sources)]
    {:sources sources
     :skeleton-base (first (keep-indexed
                            (fn [i {:keys [part]}] (when (= :body (:category part)) i))
                            sources))}))

(defn apply-material-edits
  "Apply the spec's material edits onto a composed VrmDocument. Indices are
   into the COMPOSED document's material vector (what the editor preview
   showed the user), glTF-standard baseColorFactor [r g b a]."
  [doc edits]
  (reduce (fn [d {:material/keys [index base-color]}]
            (when-not (get-in d [:gltf :materials index])
              (throw (ex-info "kisekae: material edit index out of range"
                              {:index index :materials (count (get-in d [:gltf :materials]))})))
            (assoc-in d [:gltf :materials index :pbrMetallicRoughness :baseColorFactor]
                      base-color))
          doc
          edits))

(defn apply-meta
  "Stamp the spec's name/authors/license into the document's VRM meta —
   the user's character carries THEIR meta, not the base model's (the base's
   own license must still permit this; that's a spec-authoring concern the
   consuming app surfaces, not something build can check)."
  [doc s]
  (assoc doc :meta (vt/vrm-meta {:name (:spec/name s)
                                 :authors (get-in s [:spec/meta :meta/authors])
                                 :license-url (get-in s [:spec/meta :meta/license-url])})))

(defn build-document
  "spec + docs-by-url -> composed, edited VrmDocument."
  [s docs-by-url]
  (let [ps (spec/problems s)]
    (when (seq ps) (throw (ex-info "kisekae: invalid spec" {:problems ps})))
    (let [{:keys [sources skeleton-base]} (effective-sources s docs-by-url)]
      (-> (compose/compose sources {:skeleton-base skeleton-base})
          (apply-material-edits (:spec/material-edits s))
          (apply-meta s)))))

(defn export-bytes
  "VrmDocument -> .vrm (GLB) bytes, via the engine's exporter."
  [doc]
  (export/export-glb doc))

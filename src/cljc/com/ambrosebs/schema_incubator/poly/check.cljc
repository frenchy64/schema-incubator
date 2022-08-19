(ns com.ambrosebs.schema-incubator.poly.check
  "Test functions against function schemas."
  (:require [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [com.ambrosebs.schema-incubator.poly :as poly #?@(:cljs [:refer [PolySchema]])]
            [com.gfredericks.test.chuck.properties :as prop']
            [schema-generators.generators :as sgen]
            [schema.spec.leaf :as leaf]
            [schema.spec.core :as spec]
            [schema.core :as s #?@(:cljs [:refer [FnSchema]])]
            #?(:clj [schema.macros :as macros])
            [schema-tools.walk :as walk]
            [schema.utils :as utils])
  #?(:clj (:import [schema.core FnSchema]
                   [com.ambrosebs.schema_incubator.poly PolySchema]))
  #?(:cljs (:require-macros [schema.macros :as macros])))

(declare check generator to-generative-fn-schema)

(defn fn-schema-generator
  "Generator for s/=> schemas."
  [=>-schema params]
  (let [=>-schema (to-generative-fn-schema =>-schema params)
        args-validator (s/validator (poly/args-schema =>-schema))
        return-gen (generator (poly/return-schema =>-schema) params)]
    (gen/sized
      (fn [size]
        (gen/return
          (fn [& args]
            (args-validator (vec args))
            (gen/generate return-gen size)))))))

(def +simple-leaf-generators+
  {poly/Never (gen/such-that (fn [_] (throw (ex-info "Never cannot generate values" {})))
                             gen/any)
   poly/AnyTrue (gen/such-that boolean gen/any)})

(defn default-leaf-generators
  [leaf-generators]
  (some-fn
   leaf-generators
   (sgen/default-leaf-generators
     +simple-leaf-generators+)))

(s/defn generator
  "Just like schema-generators.generators/generator, but also
  generates FnSchema's. Needed because we cannot change the s/spec for FnSchema,
  to hook into sgen/composite-generator and leaf-generators/wrappers do not
  pass params."
  ([schema] (generator schema {}))
  ([schema leaf-generators] (generator schema leaf-generators {}))
  ([schema :- sgen/Schema
    leaf-generators :- sgen/LeafGenerators
    wrappers :- sgen/GeneratorWrappers]
   (let [leaf-generators (default-leaf-generators leaf-generators)
         gen (fn [s params]
               (or (when (instance? FnSchema s) ;;TODO make composite-generator
                     (fn-schema-generator s params))
                   ((or (wrappers s) identity)
                    (or (leaf-generators s)
                        (sgen/composite-generator (s/spec s) params)))))]
     (gen/fmap
       (s/validator schema) ;; do we need to convert FnSchema to GenerativeFnSchema?
       (gen schema {:subschema-generator gen :cache #?(:clj (java.util.IdentityHashMap.)
                                                       :cljs (atom {}))})))))

(defrecord GenerativeFnSchemaSpec [fn-schema generator-args]
  spec/CoreSpec
  (subschemas [this] []) ;;?
  (checker [this params]
    (fn [x]
      (let [{:keys [passed?] :as res} (check x {:num-tests 30
                                                :schema fn-schema
                                                :generator-args generator-args})]
        (if passed?
          x
          (utils/error [x])))))
  s/HasPrecondition
  (precondition [this] ifn?)
  sgen/CompositeGenerator
  (composite-generator [this params] (fn-schema-generator fn-schema params)))

(defn- walk-fn-schema [this inner]
  (with-meta (s/->FnSchema
               (inner (:output-schema this))
               (mapv inner (:input-schemas this)))
             (meta this)))

;; same as FnSchema, except uses generative testing to validate
(macros/defrecord-schema GenerativeFnSchema [fn-schema generator-args]
  s/Schema
  (spec [this] (->GenerativeFnSchemaSpec this generator-args))
  (explain [this] (s/explain fn-schema))

  walk/WalkableSchema
  (-walk [this inner outer]
    (outer (with-meta (->GenerativeFnSchema (walk-fn-schema fn-schema inner) generator-args)
                      (meta this)))))

(extend-protocol walk/WalkableSchema
  FnSchema
  (-walk [this inner outer]
    (outer (walk-fn-schema this inner))))

(defn to-generative-fn-schema [s generator-args]
  (walk/postwalk (fn [s]
                   (if (instance? FnSchema s)
                     (with-meta (->GenerativeFnSchema s generator-args)
                                (meta s))
                     s))
                 s))

(defn check
  "Generatively test a function `f` against a FnSchema or PolySchema.

  If `f` is a qualified symbol, resolves it before checking with improved
  error messages.
  
  Takes the same options as quick-check, additionally:
  - :num-tests   number of iterations.
                 default: 100
  - :schema      the schema to check against.
                 default: (s/fn-schema f)
  Returns the same output as `quick-check`.
  
  eg., (s/defn foo :- s/Int [a :- s/Int] a)
       (check foo)"
  ([f] (check f {}))
  ([f opt]
   (let [generator (fn [s] (apply generator s (:generator-args opt)))
         to-generative-fn-schema (fn [s] (to-generative-fn-schema s (:generator-args opt)))
         qc (fn [prop]
              (apply quick-check
                     (or (:num-tests opt) 100)
                     prop
                     (apply concat (dissoc opt :schema :num-tests :leaf-generators :wrappers))))
         s (or (:schema opt)
               (s/fn-schema f))]
     (cond
       (instance? PolySchema s)
       (qc (prop'/for-all
             [insts (apply gen/tuple
                           (map (fn [[a {:keys [kind]}]]
                                  (case kind
                                    :schema (gen/one-of [(gen/return (s/eq (gensym a)))
                                                         (gen/return s/Any)])
                                    :.. (gen/one-of [(gen/vector (gen/return s/Any))
                                                     (gen/vector (gen/return s/Any))])))
                                (:parsed-decl s)))
              :let [s (to-generative-fn-schema (apply poly/instantiate s insts))]
              args (generator (poly/args-schema s))
              :let [ret-s (poly/return-schema s)
                    ret-checker (s/checker ret-s)]]
             (let [ret (apply f args)]
               (if-some [reason (ret-checker ret)]
                 (let [{:keys [error]} (macros/validation-error ret-s ret (str (utils/value-name ret)) reason)]
                   (macros/error! (utils/format* "Output of %s does not match schema: %s" (utils/fn-name f) (pr-str error))
                                  {:schema ret-s :value ret :error error}))
                 true))))

       (or (instance? GenerativeFnSchema s)
           (instance? FnSchema s))
       (let [s (to-generative-fn-schema s)
             ret-s (poly/return-schema s)
             ret-checker (s/checker ret-s)]
         (qc (prop'/for-all
               [args (generator (poly/args-schema s))]
               (let [ret (apply f args)]
                 (if-some [reason (ret-checker ret)]
                   (let [{:keys [error]} (macros/validation-error ret-s ret (str (utils/value-name ret)) reason)]
                     (macros/error! (utils/format* "Output of %s does not match schema: %s" (utils/fn-name f) (pr-str error))
                                    {:schema ret-s :value ret :error error}))
                   true)))))
       :else (throw (ex-info (str "Invalid schema to exercise: " (pr-str s))
                             {}))))))

(comment
  ; :fail [[()]],
  ; ...
  ; :smallest [[0]],
  ; ...
  ; :cause "Output of fn21868 does not match schema: \n\n\t   (not (integer? nil))  \n\n"
  (check
    @(s/defn a :- s/Int [a])
    {:num-tests 2})
  ; :message "Output of a does not match schema: \n\n\t   (not (integer? nil))  \n\n"
  )

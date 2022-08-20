(ns com.ambrosebs.schema-incubator.poly.validate
  "Schema validation using generative testing."
  (:require [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.ambrosebs.schema-incubator.poly :as poly #?@(:cljs [:refer [PolySchema]])]
            [com.gfredericks.test.chuck.properties :as prop']
            [schema-generators.generators :as sgen]
            [schema.spec.core :as spec]
            [schema.core :as s #?@(:cljs [:refer [FnSchema]])]
            #?(:clj [schema.macros :as macros])
            [schema-tools.walk :as walk]
            [schema.utils :as utils])
  #?(:clj (:import [schema.core FnSchema]
                   [com.ambrosebs.schema_incubator.poly PolySchema]))
  #?(:cljs (:require-macros [schema.macros :as macros])))

(def +simple-leaf-generators+
  {poly/Never (gen/such-that (fn [_] (throw (ex-info "Never cannot generate values" {})))
                             gen/any)
   poly/AnyTrue (gen/such-that boolean gen/any)})

(defn- default-leaf-generators
  [leaf-generators]
  (some-fn
   leaf-generators
   (sgen/default-leaf-generators
     +simple-leaf-generators+)))

(declare ^:private to-generative-fn-schema)

(defn- generator* [schema {gen :subschema-generator :as params}]
  (let [schema (to-generative-fn-schema schema params)]
    (gen/fmap
      (s/validator schema)
      (gen schema params))))

(s/defn ^:private create-generator*-params
  ([] (create-generator*-params {}))
  ([leaf-generators] (create-generator*-params leaf-generators {}))
  ([leaf-generators :- sgen/LeafGenerators
    wrappers :- sgen/GeneratorWrappers]
   (let [leaf-generators (default-leaf-generators leaf-generators)
         gen (fn [s params]
               ((or (wrappers s) identity)
                (or (leaf-generators s)
                    (sgen/composite-generator (s/spec s) params))))]
     {:subschema-generator gen :cache #?(:clj (java.util.IdentityHashMap.)
                                         :cljs (atom {}))})))

(s/defn ^:private generator
  "Just like schema-generators.generators/generator, but also
  generates FnSchema's. Needed because:
  1. we cannot change the s/spec for FnSchema, so we cannot change its sgen/composite-generator
     to be generative.
  2. nested FnSchema's like (=> (=> s/Int)) must use generative testing to check the inner one.
     since s/validate is used to check the return value of functions, we must propagate generation
     params from generator -> validator -> generator."
  ([schema] (generator schema {}))
  ([schema leaf-generators] (generator schema leaf-generators {}))
  ([schema :- sgen/Schema
    leaf-generators :- sgen/LeafGenerators
    wrappers :- sgen/GeneratorWrappers]
   (generator* schema (create-generator*-params leaf-generators wrappers))))

(defn- fn-schema-generator
  "Generator for s/=> schemas."
  [=>-schema generator*-params]
  (assert (instance? FnSchema =>-schema) (utils/type-of =>-schema))
  (let [args-validator (s/validator (poly/args-schema =>-schema))
        return-gen (generator* (poly/return-schema =>-schema) generator*-params)]
    (gen/sized
      (fn [size]
        (gen/return
          (fn [& args]
            (args-validator (vec args))
            (gen/generate return-gen size)))))))

(declare quick-validate)

(defrecord ^:internal GenerativeFnSchemaSpec [fn-schema generator*-params]
  spec/CoreSpec
  (subschemas [this] []) ;;?
  (checker [this params]
    (fn [x]
      (let [{:keys [pass?] :as res} (quick-validate x {:num-tests 30
                                                       :schema fn-schema
                                                       ::generator*-params generator*-params})]
        (if pass?
          x
          (utils/error x)))))
  s/HasPrecondition
  (precondition [this] ifn?)
  sgen/CompositeGenerator
  (composite-generator [this generator*-params] (fn-schema-generator fn-schema generator*-params)))

(defn- walk-fn-schema [this inner]
  (with-meta (s/->FnSchema
               (inner (:output-schema this))
               (mapv inner (:input-schemas this)))
             (meta this)))

;; same as FnSchema, except uses generative testing to validate
(macros/defrecord-schema ^:internal GenerativeFnSchema [fn-schema generator-params]
  s/Schema
  (spec [this] (->GenerativeFnSchemaSpec fn-schema generator-params))
  (explain [this] (s/explain fn-schema))

  walk/WalkableSchema
  (-walk [this inner outer]
    (outer (with-meta (->GenerativeFnSchema (walk-fn-schema fn-schema inner) generator-params)
                      (meta this)))))

(extend-protocol walk/WalkableSchema
  FnSchema
  (-walk [this inner outer]
    (outer (walk-fn-schema this inner))))

(defn- to-generative-fn-schema [s generator*-params]
  (walk/postwalk (fn [s]
                   (if (instance? FnSchema s)
                     (with-meta (->GenerativeFnSchema s generator*-params)
                                (meta s))
                     (if (instance? PolySchema s)
                       (update s :inst->schema (fn [inst->schema]
                                                 (let [;; don't double-wrap
                                                       original-inst->schema (or (-> inst->schema meta ::original-inst->schema)
                                                                                 inst->schema)]
                                                   (with-meta
                                                     (fn [& args]
                                                       (-> (apply original-inst->schema args)
                                                           (to-generative-fn-schema generator*-params)))
                                                     {::original-inst->schema original-inst->schema}))))
                       s)))
                 s))

(defn quick-validate
  "Validate a value against a schema via generative testing.

  Takes the same options as quick-check, additionally:
  - :num-tests   number of iterations.
                 default: 100
  - :schema      the schema to validate against.
                 default: (s/fn-schema f)
  - :generator-args  a vector of arguments you would normally pass to `schema-generators.generators/generator`.
  Returns the same output as `quick-check`.
  
  eg., (s/defn foo :- s/Int [a :- s/Int] a)
       (quick-validate foo)
       (quick-validate foo {:schema (s/=> s/Num s/Num)
                            :num-tests 100
                            :generator-args [my-leaf-generators my-wrappers]})"
  ([f] (quick-validate f {}))
  ([f opt]
   (let [generator*-params (or (::generator*-params opt) ;; reuse params in nested validate->generate->validate->generate...
                               (apply create-generator*-params (:generator-args opt)))
         generator (fn [s] (generator* s generator*-params))
         ;; when =>'s are nested, the inner ones will be checked via quick-validate (via fn-schema-generator -> poly/args-schema -> s/validator,
         ;; or generator* -> gen/fmap -> s/validator).
         ;; we need to stash the :generator*-params in that case so can be used later---we do that by
         ;; wrapping FnSchema with GenerativeFnSchema.
         to-generative-fn-schema (fn [s] (to-generative-fn-schema s generator*-params))
         qc (fn [prop]
              (apply quick-check
                     (or (:num-tests opt) 100)
                     prop
                     (apply concat (dissoc opt :schema :num-tests :leaf-generators :wrappers))))
         s (some-> (or (:schema opt)
                       (s/fn-schema f))
                   to-generative-fn-schema)]
     ;; idea: these should be integrated into the `spec` of these schemas. that way,
     ;; generative testing isn't special, but merely an option of normal validation.
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
              :let [s (apply poly/instantiate s insts)]
              args (generator (poly/args-schema s))
              :let [ret-s (poly/return-schema s)
                    ret-checker (s/checker ret-s)]]
             (let [ret (apply f args)]
               (if-some [reason (ret-checker ret)]
                 (let [{:keys [error]} (macros/validation-error ret-s ret (str (utils/value-name ret)) reason)]
                   (macros/error! (utils/format* "Output of %s does not match schema: %s" (utils/fn-name f) (pr-str error))
                                  {:schema ret-s :value ret :error error}))
                 true))))

       (instance? GenerativeFnSchema s)
       (let [ret-s (poly/return-schema s)
             ret-checker (s/checker ret-s)]
         (qc (prop'/for-all
               [args (generator (poly/args-schema s))]
               (let [ret (apply f args)]
                 (if-some [reason (ret-checker ret)]
                   (let [{:keys [error]} (macros/validation-error ret-s ret (str (utils/value-name ret)) reason)]
                     (macros/error! (utils/format* "Output of %s does not match schema: %s" (utils/fn-name f) (pr-str error))
                                    {:schema ret-s :value ret :error error}))
                   true)))))
       ;:else (s/validate s f)  ; kind of makes sense, except we need to figure out how to a return quick-check style summary.
       :else (throw (ex-info (str "Invalid schema to exercise: " (pr-str s))
                             {}))))))

(comment
  ; :fail [[()]],
  ; ...
  ; :smallest [[0]],
  ; ...
  ; :cause "Output of fn21868 does not match schema: \n\n\t   (not (integer? nil))  \n\n"
  (quick-validate
    @(s/defn a :- s/Int [a])
    {:num-tests 2})
  ; :message "Output of a does not match schema: \n\n\t   (not (integer? nil))  \n\n"
  )

(ns com.ambrosebs.schema-incubator.poly.validate
  "Schema validation using generative testing."
  (:require [clojure.walk :as cwalk]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.ambrosebs.schema-incubator.poly :as poly #?@(:cljs [:refer [PolySchema]])]
            [com.gfredericks.test.chuck.properties :as prop']
            [schema-generators.generators :as sgen]
            [schema.spec.core :as spec]
            [schema.core :as s #?@(:cljs [:refer [FnSchema]])]
            #?(:clj [schema.macros :as macros])
            [schema-tools.walk :as swalk]
            [schema.utils :as utils])
  #?(:clj (:import [schema.core FnSchema]
                   [com.ambrosebs.schema_incubator.poly PolySchema]))
  #?(:cljs (:require-macros [schema.macros :as macros])))

(def +simple-leaf-generators+
  {poly/NeverOutput (gen/such-that (fn [_] (throw (ex-info "NeverOutput cannot generate values" {})))
                                   gen/any)
   poly/NeverInput (gen/return poly/NeverInput)
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

;; checks input, generates output (opposite of quick-validate)
(defn- fn-schema-generator
  "Generator for s/=> schemas."
  [=>-schema generator*-params]
  (assert (instance? FnSchema =>-schema) (utils/type-of =>-schema))
  (let [args-validator (s/validator (poly/args-schema =>-schema))
        return-gen (generator* (poly/return-schema =>-schema) generator*-params)]
    (gen/sized
      (fn [size]
        (gen/return
          (-> (fn [& args]
                (args-validator (vec args))
                (gen/generate return-gen size))
              (with-meta {:schema =>-schema})))))))

(declare quick-validate check)

(defrecord ^:internal GenerativeFnSchemaSpec [fn-schema generator*-params error-form-constructor]
  spec/CoreSpec
  (subschemas [this] []) ;;?
  (checker [this params];;hmm do we propagate checker params via generator->checker->generator->checker...?
    (fn [x]
      (check x {:num-tests 30
                :schema fn-schema
                ::generator*-params generator*-params
                ::error-form-constructor error-form-constructor})
      #_
      (let [{:keys [pass?] :as res} (quick-validate x {:num-tests 30
                                                       :schema fn-schema
                                                       ::generator*-params generator*-params
                                                       ::error-form-constructor error-form-constructor})]
        (when-not pass?
          (utils/error
            (error-form-constructor 'BAD_FIXME)
            #_res)))))
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
(macros/defrecord-schema ^:internal GenerativeFnSchema [fn-schema generator-params error-form-constructor]
  s/Schema
  (spec [this] (->GenerativeFnSchemaSpec fn-schema generator-params error-form-constructor))
  (explain [this] (with-meta (s/explain fn-schema)
                             {:com.ambrosebs.schema-incubator.poly/explain-schema this}))

  swalk/WalkableSchema
  (-walk [this inner outer]
    (outer (with-meta (->GenerativeFnSchema (walk-fn-schema fn-schema inner) generator-params error-form-constructor)
                      (meta this)))))

(extend-protocol swalk/WalkableSchema
  FnSchema
  (-walk [this inner outer]
    (outer (walk-fn-schema this inner))))

(defn- to-generative-fn-schema 
  ([s generator*-params] (to-generative-fn-schema s generator*-params identity))
  ([s generator*-params base-error-form-constructor]
   (let [created-schema->error-form-constructor (atom {})
         s (swalk/postwalk (fn [s]
                             (if (instance? FnSchema s)
                               (let [error-form-constructor (volatile! nil)
                                     s (with-meta (->GenerativeFnSchema s generator*-params (fn [inner-error-form] (@error-form-constructor inner-error-form)))
                                                  (meta s))]
                                 (swap! created-schema->error-form-constructor assoc s error-form-constructor)
                                 s)
                               (if (instance? PolySchema s)
                                 (let [error-form-constructor (volatile! nil)
                                       s (update s :inst->schema (fn [inst->schema]
                                                                   (let [;; don't double-wrap
                                                                         original-inst->schema (or (-> inst->schema meta ::original-inst->schema)
                                                                                                   inst->schema)]
                                                                     (with-meta
                                                                       (fn [& args]
                                                                         (-> (apply original-inst->schema args)
                                                                             (to-generative-fn-schema
                                                                               generator*-params
                                                                               (fn [inner-error-form]
                                                                                 (@error-form-constructor inner-error-form)))))
                                                                       {::original-inst->schema original-inst->schema}))))]
                                   (swap! created-schema->error-form-constructor assoc s error-form-constructor)
                                   s)
                                 s)))
                           s)
         explain-s (s/explain s)]
     (doseq [[s error-form-constructor] @created-schema->error-form-constructor]
       (vreset! error-form-constructor (fn [inner-error-form]
                                         (base-error-form-constructor
                                           (cwalk/postwalk (fn [inner-explain]
                                                             (if (identical? s (-> inner-explain meta :com.ambrosebs.schema-incubator.poly/explain-schema))
                                                               inner-error-form
                                                               inner-explain))
                                                           explain-s)))))
     s)))

;; generates input, checks output (opposite of fn-schema-generator)
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
         to-generative-fn-schema (fn [s] (to-generative-fn-schema s generator*-params
                                                                  (or (::error-form-constructor opt) identity)))
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
       (let [res (qc (prop'/for-all
                       [insts (apply gen/tuple
                                     (map (fn [[a {:keys [kind]}]]
                                            (case kind
                                              :schema (gen/one-of [(gen/return (s/eq a)) ;; shrink to readable value
                                                                   (gen/return (s/eq (gensym a)))
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
                           (let [{:keys [error]} (macros/validation-error ret-s ret (utils/value-name ret) reason)]
                             (macros/error! (utils/format* "Output of %s does not match schema: %s" (utils/fn-name f) (pr-str error))
                                            {:schema ret-s :value ret :error error}))
                           true))))]
         (cond-> res
           ;;TODO figure out what to do here
           (not (:pass? res)) (assoc ::explain-error (list 'not (-> res :shrunk :smallest first (get 's) s/explain)))))

       (instance? GenerativeFnSchema s)
       (let [ret-s (poly/return-schema s)
             ret-checker (s/checker ret-s)
             res (qc (prop'/for-all
                       [args (generator (poly/args-schema s))]
                       (let [ret (apply f args)]
                         (if-some [reason (ret-checker ret)]
                           (let [{:keys [error]} (macros/validation-error ret-s ret (str (utils/value-name ret)) reason)]
                             (macros/error! (utils/format* "Output of %s does not match schema: %s" (utils/fn-name f) (pr-str error))
                                            {:schema ret-s :value ret :error error}))
                           true))))]
         (cond-> res
           ;;TODO figure out what to do here
           (not (:pass? res)) (assoc ::explain-error (list 'not (s/explain s)))))
       ;:else (s/validate s f)  ; kind of makes sense, except we need to figure out how to a return quick-check style summary.
       :else (throw (ex-info (str "Invalid schema to exercise: " (pr-str s))
                             {}))))))

(defn check [& args]
  (let [{:keys [pass?] :as result} (apply quick-validate args)]
    (when-not pass?
      (::explain-error result)
      #_result
      #_
      (let [smallest (-> result :shrunk :smallest first)]
        (if ('insts smallest)
          ;; poly case
          (list 'not (s/explain ('s smallest)))
          smallest)))))

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

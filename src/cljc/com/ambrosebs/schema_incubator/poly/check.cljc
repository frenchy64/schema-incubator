(ns com.ambrosebs.schema-incubator.poly.check
  (:require [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [com.ambrosebs.schema-incubator.poly :as poly #?@(:cljs [:refer [PolySchema]])]
            [com.gfredericks.test.chuck.properties :as prop']
            [schema-generators.generators :as sgen]
            [schema.core :as s #?@(:cljs [:refer [FnSchema]])]
            #?(:clj [schema.macros :as macros])
            [schema.utils :as utils])
  #?(:clj (:import [schema.core FnSchema]
                   [com.ambrosebs.schema_incubator.poly PolySchema]))
  #?(:cljs (:require-macros [schema.macros :as macros])))

(declare generator)

(defn- fn-schema-generator
  "Generator for s/=> schemas."
  [=>-schema params]
  (let [args-validator (s/validator (poly/args-schema =>-schema))
        return-gen (generator (poly/return-schema =>-schema) params)]
    (gen/sized
      (fn [size]
        (gen/return
          (fn [& args]
            (args-validator (vec args))
            (gen/generate return-gen size)))))))

(s/defn generator
  "Just like schema-generators.generators/generator, but also
  generates FnSchema's."
  ([schema] (generator schema {}))
  ([schema leaf-generators] (generator schema leaf-generators {}))
  ([schema :- sgen/Schema
    leaf-generators :- sgen/LeafGenerators
    wrappers :- sgen/GeneratorWrappers]
   (let [leaf-generators (sgen/default-leaf-generators leaf-generators)
         gen (fn [s params]
               (or (when (instance? FnSchema s)
                     (fn-schema-generator s params))
                   ((or (wrappers s) identity)
                    (or (leaf-generators s)
                        (sgen/composite-generator (s/spec s) params)))))]
     (prn "generator" (str schema))
     (gen/fmap
       (s/validator schema)
       (gen schema {:subschema-generator gen :cache #?(:clj (java.util.IdentityHashMap.)
                                                       :cljs (atom {}))})))))

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
                                    :schema (generator s/Any)
                                    :.. (gen/vector (generator s/Any))))
                                (:parsed-decl s)))
              :let [s (apply poly/instantiate s insts)]
              args (generator (poly/args-schema s))]
             (do (s/validate
                   (poly/return-schema s)
                   (apply f args))
                 true)))

       (instance? FnSchema s)
       (let [ret-s (poly/return-schema s)
             ret-checker (s/checker ret-s)]
         (qc (prop'/for-all
               [args (generator (poly/args-schema s))]
               (let [ret (apply f args)]
                 (if-some [reason (ret-checker ret)]
                   (let [{:keys [error]} (macros/validation-error ret-s ret (str (utils/value-name ret)) reason)]
                     (macros/error! (utils/format* "Output of %s does not match schema: %s" f (pr-str error))
                                    {:schema ret-s :value ret :error error}) )
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

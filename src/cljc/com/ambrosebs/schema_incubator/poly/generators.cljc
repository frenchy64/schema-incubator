(ns com.ambrosebs.schema-incubator.poly.generators
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
  generates FnSchema's."
  ([schema] (generator schema {}))
  ([schema leaf-generators] (generator schema leaf-generators {}))
  ([schema :- sgen/Schema
    leaf-generators :- sgen/LeafGenerators
    wrappers :- sgen/GeneratorWrappers]
   (let [leaf-generators (default-leaf-generators leaf-generators)
         gen (fn [s params]
               (or (when (instance? FnSchema s)
                     (fn-schema-generator s params))
                   ((or (wrappers s) identity)
                    (or (leaf-generators s)
                        (sgen/composite-generator (s/spec s) params)))))]
     (gen/fmap
       (s/validator schema)
       (gen schema {:subschema-generator gen :cache #?(:clj (java.util.IdentityHashMap.)
                                                       :cljs (atom {}))})))))

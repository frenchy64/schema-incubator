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
  (when (instance? FnSchema =>-schema)
    (let [args-validator (s/validator (poly/args-schema =>-schema))
          return-gen (generator (poly/return-schema =>-schema) params)]
      (gen/sized
        (fn [size]
          (gen/return
            (fn [& args]
              (args-validator (vec args))
              (gen/generate return-gen size))))))))

(def +simple-leaf-generators+
  {poly/Never (gen/such-that (fn [_] (throw (ex-info "Never cannot generate values" {})))
                             gen/any)
   poly/AnyTrue (gen/such-that boolean gen/any)})

(defn default-leaf-generators
  [leaf-generators]
  (sgen/default-leaf-generators
    (some-fn
      leaf-generators
      fn-schema-generator
      +simple-leaf-generators+)))

(s/defn generator
  "Just like schema-generators.generators/generator, but also
  generates FnSchema's."
  ([schema] (generator schema {}))
  ([schema leaf-generators] (generator schema leaf-generators {}))
  ([schema :- sgen/Schema
    leaf-generators :- sgen/LeafGenerators
    wrappers :- sgen/GeneratorWrappers]
   (let [leaf-generators (default-leaf-generators leaf-generators)]
     (sgen/generator schema leaf-generators wrappers))))

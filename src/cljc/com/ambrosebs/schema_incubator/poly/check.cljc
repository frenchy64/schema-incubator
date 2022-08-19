(ns com.ambrosebs.schema-incubator.poly.check
  "Test functions against function schemas."
  (:require [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [com.ambrosebs.schema-incubator.poly :as poly #?@(:cljs [:refer [PolySchema]])]
            [com.ambrosebs.schema-incubator.poly.generators :refer [generator]]
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

(declare check)

;; same as FnSchema, except uses generative testing to validate
(macros/defrecord-schema GenerativeFnSchema [output-schema input-schemas] ;; input-schemas sorted by arity
  s/Schema
  (spec [this] (leaf/leaf-spec (spec/simple-precondition this #(check % {:schema this}))))
  (explain [this]
    (if (> (count input-schemas) 1)
      (list* '=>* (s/explain output-schema) (map s/explain-input-schema input-schemas))
      (list* '=> (s/explain output-schema) (s/explain-input-schema (first input-schemas)))))

  walk/WalkableSchema
  (-walk [this inner outer]
    (outer (with-meta (->GenerativeFnSchema
                        (inner (:output-schema this))
                        (mapv inner (:input-schemas this)))
                      (meta this)))))

(extend-protocol walk/WalkableSchema
  FnSchema
  (-walk [this inner outer]
    (outer (with-meta (s/make-fn-schema
                        (inner (:output-schema this))
                        (mapv inner (:input-schemas this)))
                      (meta this)))))

(defn to-generative-fn-schema [s]
  (walk/postwalk (fn [s]
                   (if (instance? FnSchema s)
                     (with-meta (->GenerativeFnSchema (:output-schema s) (:input-schemas s))
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
                 (let [{:keys [error]} (macros/validation-error ret-s ret (str (utils/fn-name ret)) reason)]
                   (macros/error! (utils/format* "Output of %s does not match schema: %s" f (pr-str error))
                                  {:schema ret-s :value ret :error error}) )
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
                   (let [{:keys [error]} (macros/validation-error ret-s ret (str (utils/fn-name ret)) reason)]
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

(ns com.ambrosebs.schema-incubator.poly.check
  (:require [clojure.test.check :refer [quick-check]]
            [schema-generators.generators :as sgen]
            [com.gfredericks.test.chuck.properties :as prop']
            [clojure.test.check.generators :as gen]
            [com.ambrosebs.schema-incubator.poly :as poly]
            [schema.core :as s]))

(defn check
  "Generatively test a function `f` against a FnSchema or PolySchema.
  
  Takes the same options as quick-check, additionally:
  - :run-tests   number of iterations.
                 default: 100
  - :schema      the schema to check against.
                 default: (s/fn-schema f)
  Returns the same output as `quick-check`.
  
  eg., (s/defn foo :- s/Int [a :- s/Int] a)
       (check foo)"
  ([f] (check f {}))
  ([f opt]
   (let [qc (fn [prop]
              (apply quick-check
                     (or (:num-tests opt) 100)
                     prop
                     (apply concat (dissoc opt :schema :num-tests))))
         s (or (:schema opt)
               (s/fn-schema f))]
     (cond
       (instance? schema.core.FnSchema s)
       (qc (prop'/for-all
             [insts (apply gen/tuple
                           (map (fn [a]
                                  (if (-> a meta :nat)
                                    ;; TODO gen nats too
                                    (gen/return :any-nat)
                                    ;; TODO make a spec subtype hierarchy
                                    (sgen/generator s/Any)))
                                (:decl s)))
              :let [s (apply poly/instantiate s insts)]
              args (sgen/generator (poly/args-schema s))]
             (do (s/validate
                   (poly/return-schema s)
                   (apply f args))
                 true)))

       (instance? schema.core.FnSchema s)
       (qc (prop'/for-all
             [args (sgen/generator (poly/args-schema s))]
             (do (s/validate
                   (poly/return-schema s)
                   (apply f args))
                 true)))
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

;; goes last

(defn generator
  "Generator for s/=> schemas."
  [=>-schema]
  (let [args-schema (poly/args-schema =>-schema)
        return-gen (sgen/generator (poly/return-schema =>-schema))]
    (gen/sized
      (fn [size]
        (gen/return
          (fn [& args]
            (s/validate
              args-schema
              (vec args))
            (gen/generate return-gen size)))))))

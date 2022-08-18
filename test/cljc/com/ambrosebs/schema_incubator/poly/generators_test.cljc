(ns com.ambrosebs.schema-incubator.poly.generators-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [com.ambrosebs.schema-incubator.poly :as poly :refer [=> all]]
            [com.ambrosebs.schema-incubator.poly.generators :as sut]
            [schema-generators.generators :as sgen]
            [schema.core :as s]))

(deftest generator-test
  (is (integer? ((gen/generate
                   (sut/generator (s/=> s/Int s/Int)))
                 1)))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               ((gen/generate
                  (sut/generator (s/=> s/Int s/Int)))
                :foo))))

;; TODO improve blame msg (should be 'something here, not arg0)
(comment
  ((gen/generate
     (sut/generator (s/=> s/Int (s/named s/Int 'something))))
   :foo)
  ;=> Value does not match schema: [(named (named (not (integer? :foo)) something) arg0)]
  (sut/generator s/Int)
  (s/validate {:a s/Str} {:a 1})
  )

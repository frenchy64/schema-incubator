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

(deftest Never-gen-test
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Never cannot generate values"
                        (gen/generate (sut/generator poly/Never))))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Value does not match schema: \[\(named \(not \(Never 1\)\) arg0\)\]"
                        ((gen/generate (sut/generator (s/=> s/Any poly/Never))) 1))))

(deftest AnyTrue-gen-test
  (is (gen/generate (sut/generator poly/AnyTrue))))

(deftest AnyFalse-gen-test
  (is (not (gen/generate (sut/generator poly/AnyFalse)))))

;; TODO improve blame msg (should be 'something here, not arg0)
(comment
  ((gen/generate
     (sut/generator (s/=> s/Int (s/named s/Int 'something))))
   :foo)
  ;=> Value does not match schema: [(named (named (not (integer? :foo)) something) arg0)]
  (sut/generator s/Int)
  (s/validate {:a s/Str} {:a 1})
  )

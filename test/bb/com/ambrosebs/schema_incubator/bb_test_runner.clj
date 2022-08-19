(ns com.ambrosebs.schema-incubator.bb-test-runner
  (:require [clojure.test :as t]
            [babashka.classpath :as cp]))

(def test-nsyms '[com.ambrosebs.schema-incubator.poly-test
                  com.ambrosebs.schema-incubator.poly.validate-test
                  com.ambrosebs.schema-incubator.poly.macros-test])

(apply require test-nsyms)

(def test-results
  (apply t/run-tests test-nsyms))

(def failures-and-errors
  (let [{:keys [:fail :error]} test-results]
    (+ fail error)))

(System/exit failures-and-errors)

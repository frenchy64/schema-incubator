(ns com.ambrosebs.schema-incubator.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            com.ambrosebs.schema-incubator.poly-test
            com.ambrosebs.schema-incubator.poly.validate-test))

(doo-tests
  'com.ambrosebs.schema-incubator.poly-test
  'com.ambrosebs.schema-incubator.poly.validate-test)

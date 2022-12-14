(ns com.ambrosebs.schema-incubator.poly.macros-test
  ;schema.macros-test
  (:use clojure.test)
  (:require
   [schema.core :as s]
   [com.ambrosebs.schema-incubator.poly.macros :as macros]))

(deftest extract-leading-fn-kv-pairs-test
  (is (= (macros/extract-leading-fn-kv-pairs [])
         [{} []]))
  (is (= (macros/extract-leading-fn-kv-pairs ['name :- 'schema])
         [{} ['name :- 'schema]]))
  (is (= (macros/extract-leading-fn-kv-pairs [:all '[x] 'name :- 'schema])
         [{::macros/poly-binder '[x]} ['name :- 'schema]]))
  (is (= (macros/extract-leading-fn-kv-pairs [:- '[s/Any] :- 'schema])
         [{} [:- '[s/Any] :- 'schema]]))
  (is (= (macros/extract-leading-fn-kv-pairs [:all '[x] :- '[s/Any] :- 'schema])
         [{::macros/poly-binder '[x]} [:- '[s/Any] :- 'schema]])))

(deftest normalized-defn-args-test
  (doseq [explicit-meta [{} {:a -1 :c 3}]
          [leading-map leading-forms] {{} []
                                       '{::macros/poly-binder [x]} '[:all [x]]}
          [schema-attrs schema-forms] {{:schema `s/Any} []
                                       {:schema 'Long :tag 'Long} [:- 'Long]}
          [doc-attrs doc-forms] {{} []
                                 {:doc "docstring"} ["docstring"]}
          [attr-map attr-forms] {{} {}
                                 {:a 1 :b 2} [{:a 1 :b 2}]}]
    (let [simple-body ['[x] `(+ 1 1)]
          full-args (concat leading-forms [(with-meta 'abc explicit-meta)] schema-forms doc-forms attr-forms simple-body)
          [name & more] (macros/normalized-defn-args {} full-args)]
      (testing (vec full-args)
        (is (= (concat ['abc (merge explicit-meta schema-attrs doc-attrs attr-map leading-map) simple-body])
               (concat [name (meta name) more])))))))

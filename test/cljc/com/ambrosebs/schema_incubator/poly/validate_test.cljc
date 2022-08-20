(ns com.ambrosebs.schema-incubator.poly.validate-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [com.ambrosebs.schema-incubator.poly :as poly :refer [=> all]]
            [com.ambrosebs.schema-incubator.poly.validate :as sut]
            [schema-generators.generators :as sgen]
            [schema.core :as s]
            [schema.utils :as utils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generators

(deftest generator-test
  (is (integer? ((gen/generate
                   (@#'sut/generator (s/=> s/Int s/Int)))
                 1)))
  (is (thrown? #?(:clj Exception :cljs js/Error)
               ((gen/generate
                  (@#'sut/generator (s/=> s/Int s/Int)))
                :foo))))

(deftest Never-gen-test
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Never cannot generate values"
                        (gen/generate (@#'sut/generator poly/Never))))
  ;; hmm
  ;  (=> Never Any) <: (=> Any Never)
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Value does not match schema: \[\(named \(not \(Never 1\)\) arg0\)\]"
                        (gen/generate (@#'sut/generator (s/=> s/Any poly/Never)))))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Value does not match schema: \[\(named \(not \(Never 1\)\) arg0\)\]"
                        ((gen/generate (@#'sut/generator (s/=> s/Any poly/Never))) 1))))

(deftest AnyTrue-gen-test
  (is (gen/generate (@#'sut/generator poly/AnyTrue))))

(deftest AnyFalse-gen-test
  (is (not (gen/generate (@#'sut/generator poly/AnyFalse)))))

;; TODO improve blame msg (should be 'something here, not arg0)
(comment
  ((gen/generate
     (@#'sut/generator (s/=> s/Int (s/named s/Int 'something))))
   :foo)
  ;=> Value does not match schema: [(named (named (not (integer? :foo)) something) arg0)]
  (@#'sut/generator s/Int)
  (s/validate {:a s/Str} {:a 1})
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Validation

(deftest validate-test
  (testing "success"
    (let [atm (atom [])
          res (sut/quick-validate
                (poly/fn [a :- s/Int] (swap! atm conj a) a)
                {:num-tests 2
                 :seed 1634677540521})]
      (is (= {:result true, :pass? true, :num-tests 2, :seed 1634677540521}
             (dissoc res :time-elapsed-ms)))
      (is (= [-1N 0] @atm))))
  (testing "failure"
    (let [res (sut/quick-validate
                (poly/fn foo :- s/Int [a])
                {:num-tests 2
                 :seed 1634677540521})]
      (is (= {:pass? false
              :seed 1634677540521
              :num-tests 1}
             (select-keys res #{:pass? :seed :num-tests :cause})))
      (is (= {:smallest '[{args [0]}]}
             (select-keys (:shrunk res) #{:smallest})))
      (is (re-find #"Output of .*foo.* does not match schema.*"
                   (-> res
                       (get-in [:shrunk :result])
                       .getMessage)))
      (is (= {:type :schema.core/error, :schema s/Int, :value nil}
             (-> res
                 (get-in [:shrunk :result])
                 ex-data
                 (select-keys #{:type :schema :value})))))))

(deftest poly-validate-test
  (is (true? (:pass? (sut/quick-validate (poly/fn :all [X] _id :- X [a :- X] a)))))
  (is (false? (:pass? (sut/quick-validate (poly/fn :all [X] _not-id :- X [a :- X] 1)))))
  (is (false? (:pass? (sut/quick-validate (poly/fn _not-id [a] 1)
                                 {:schema (poly/all [X] (poly/=> X X))}))))
  (is (true? (:pass? (sut/quick-validate
                       (poly/fn :- s/Int
                         [a :- s/Int] a)))))
  (is (false? (:pass? (sut/quick-validate
                        (poly/fn :- s/Int
                          [a :- s/Int] (str a))))))
  (is (true? (:pass? (sut/quick-validate
                       (poly/fn; :- (s/=> s/Int s/Int)
                         [a :- (s/=> s/Int s/Int)] a)))))
  (is (true? (:pass? (sut/quick-validate
                       (poly/fn :- (poly/=> s/Int s/Int)
                         [a :- (poly/=> s/Int s/Int)] a)))))
  #_ ;;TODO
  (sut/quick-validate
    (poly/fn :all [X :..] :- (poly/=> X X :.. X)
      [a :- (poly/=> X X :.. X)] a))
  )

;; partial
(all [X :.. Y :.. Z]
     (=> (=> Z Y :.. Y)
         (=> Z X :.. X Y :.. Y)
         X :.. X))

every-pred
(all [X :.. Y]
     (=> (=> s/Bool & [X])
         (=> s/Any X)
         & [(=> s/Any X)]
         ))

;; comp
(all [X :.. Y0]
     (=> (=> Y0 X :.. X)
         (=> Y0 X :.. X)))
(all [X :.. Y0 Y1 Z]
     (=> (=> Y1 X :.. X)
         (=> Y1 Y0)
         (=> Y0 X :.. X)))
(all [X :.. Y0 Y1 Y2 Z]
     (=> (=> Y2 X :.. X)
         (=> Y2 Y1)
         (=> Y1 Y0)
         (=> Y0 X :.. X)))
(all [X :.. Y0 Y1 Y2 Y3 Z]
     (=> (=> Y3 X :.. X)
         (=> Y3 Y2)
         (=> Y2 Y1)
         (=> Y1 Y0)
         (=> Y0 X :.. X)))

; comp
#_
(all [X :.. Y :- [:.. :min 1]]
     {:no-general-inst true}
     (=> (=> (first Y) X :.. X)
         (=> (first Y) (second Y)) :..app Y (partition-all 2 Y 1)
         (=> (poly/...app Y (peek Y)) X :.. X)))

; comp
#_
(all [X :.. Y :- [:.. :min 1]]
     {:no-general-inst true}
     (s/make-fn-schema
       (=> (first Y) X :.. X)
       (->> (partition-all 2 Y 1)
            (mapv (fn [[ret arg]] (s/one (=> ret arg) (gensym))))
            (conj (s/one (=> (peek Y) X :.. X) (gensym))))))

; every-pred
#_
(all [X :.. Y]
     (=> (=> s/Bool & [X])
         (=> s/Any X)
         & [(=> s/Any X)]))

(deftest Never-test
  (is (s/check poly/Never false)))

(def every-pred-short-circuits-schema
  "Tests that every-pred short-circuits on the first failed predicate.
  
  TODO: need to define how (=> s/Bool poly/Never) should validate before this works."
  (all [X Y :.. Z :..]
       (s/make-fn-schema 
         (=> (s/eq false) X)
         [(-> (mapv (fn [_] (s/one (=> poly/AnyTrue X) (gensym))) Y)
              (conj (s/one (=> poly/AnyFalse X) (gensym)))
              (into (map (fn [_] (s/one (=> s/Bool poly/Never) (gensym)))) Z))])))

(deftest every-pred-short-circuits-test
  (testing "schema expands as expected"
    (is (= '(=> (=> (eq false) Int)
                (=> (enum nil false) Int))
           (s/explain (poly/instantiate every-pred-short-circuits-schema s/Int [] []))))
    (is (= '(=> (=> (eq false) Int)
                (=> (pred AnyTrue) Int) ;; pass
                (=> (enum nil false) Int) ;; fail
                (=> Bool (pred Never))) ;; short-circuit
           (s/explain (poly/instantiate every-pred-short-circuits-schema s/Int [1] [2])))))
  (is (:pass? (sut/quick-validate
                every-pred
                {:schema (poly/instantiate every-pred-short-circuits-schema
                                           s/Int [] [])})))
  (is (:pass? (sut/quick-validate
                every-pred
                {:schema (poly/instantiate every-pred-short-circuits-schema
                                           s/Int [s/Any] [])})))
  (is (:pass? (sut/quick-validate
                every-pred
                {:schema (poly/instantiate every-pred-short-circuits-schema
                                           s/Int [s/Any s/Any s/Any] [])})))
  #_ ;;TODO don't validate (=> Any Never) immediately following generation
  (is (:pass? (sut/quick-validate every-pred {:schema every-pred-short-circuits-schema})))
  #_ ;;TODO don't validate (=> Any Never) immediately following generation
  (is (not (:pass? (sut/quick-validate (fn [& fs]
                                         (fn [& args]
                                           (doseq [f fs
                                                   arg args]
                                             (f arg))
                                           (apply (apply every-pred fs) args)))
                                       {:schema every-pred-short-circuits-schema})))))

(def comp-schema
  "All valid ways of calling comp and using its result."
  (all [X :.. Y :.. Z]
       (let [Y (conj Y Z)]
         (s/make-fn-schema
           (=> (first Y) X :.. X)
           [(-> (mapv (fn [[ret arg]] (s/one (=> ret arg) (gensym)))
                      (partition 2 1 Y))
                (conj (s/one (=> (peek Y) X :.. X) (gensym))))]))))

(deftest comp-schema-test
  (testing "schema expands as expected"
    (is (= '(=> (=> (eq :X))
                (=> (eq :X)))
           (s/explain (poly/instantiate comp-schema [] [] (s/eq :X)))))
    (is (= '(=> (=> (eq :X) (eq :Y0))
                (=> (eq :X) (eq :Y0)))
           (s/explain (poly/instantiate comp-schema [(s/eq :Y0)] [] (s/eq :X)))))
    (is (= '(=> (=> (eq :X) (eq :Y0) (eq :Y1))
                (=> (eq :X) (eq :Y0) (eq :Y1)))
           (s/explain (poly/instantiate comp-schema [(s/eq :Y0) (s/eq :Y1)] [] (s/eq :X)))))
    (is (= '(=> (=> (eq :Z))
                (=> (eq :Z) (eq :X))
                (=> (eq :X)))
           (s/explain (poly/instantiate comp-schema [] [(s/eq :Z)] (s/eq :X)))))
    (is (= '(=> (=> (eq :Z) (eq :Y0) (eq :Y1))
                (=> (eq :Z) (eq :X))
                (=> (eq :X) (eq :Y0) (eq :Y1)))
           (s/explain (poly/instantiate comp-schema [(s/eq :Y0) (s/eq :Y1)] [(s/eq :Z)] (s/eq :X)))))
    (is (= '(=> (=> (eq :X0) (eq :Y0) (eq :Y1))
                (=> (eq :X0) (eq :X1))
                (=> (eq :X1) (eq :X2))
                (=> (eq :X2) (eq :X3))
                (=> (eq :X3) (eq :Y0) (eq :Y1)))
           (s/explain (poly/instantiate comp-schema [(s/eq :Y0) (s/eq :Y1)] [(s/eq :X0) (s/eq :X1) (s/eq :X2)] (s/eq :X3)))))
    (is (:pass? (sut/quick-validate
                  comp
                  {:schema (poly/instantiate comp-schema [] [] [(s/eq :X)])}))))
  (is (:pass? (sut/quick-validate
                comp
                {:schema (poly/instantiate comp-schema [(s/eq :Y0) (s/eq :Y1)] [(s/eq :X0) (s/eq :X1) (s/eq :X2)] (s/eq :X3))})))
  (is (false? (:pass? (sut/quick-validate
                        every-pred
                        {:schema (poly/instantiate comp-schema [(s/eq :Y0) (s/eq :Y1)] [(s/eq :X0) (s/eq :X1) (s/eq :X2)] (s/eq :X3))}))))
  ;;slow
  (is (:pass? (sut/quick-validate
                comp
                {:schema comp-schema
                 :num-tests 30}))))

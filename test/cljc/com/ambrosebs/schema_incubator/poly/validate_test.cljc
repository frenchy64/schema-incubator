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

(deftest NeverOutput-test
  (is (s/check poly/NeverOutput 1))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"NeverOutput cannot generate values"
                        (gen/generate (@#'sut/generator poly/NeverOutput))))
  ;; hmm
  ;  (=> Never Any) <: (=> Any Never)
  #_ ;;TODO
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Value does not match schema: .*" ;;shrunk...
                        (gen/generate (@#'sut/generator (s/=> poly/NeverOutput s/Any)))))
  )

(deftest NeverInput-test
  (is (gen/generate (@#'sut/generator (s/=> s/Any poly/NeverInput))))
  (is (s/validate (s/=> s/Any poly/NeverInput) identity)) ;; unsure if this is a good thing
  (is ((gen/generate (@#'sut/generator (s/=> poly/NeverInput s/Any))) 1)) ;; undesirable use of NeverInput
  (is (s/validate (s/=> s/Any poly/NeverInput)
                  (gen/generate (@#'sut/generator (s/=> s/Any poly/NeverInput)))))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error)
                        #"Value does not match schema: \[\(named \(not \(= NeverInput.*"
                        ((gen/generate (@#'sut/generator (s/=> s/Any poly/NeverInput))) 1))))

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
  ; (=> (not (= X 1)) (eq X))
  (is (false? (:pass? (sut/quick-validate (poly/fn :all [X] _not-id :- X [a :- X] 1)))))
  ; (=> (not (= X 1)) (eq X))
  (is (false? (:pass? (sut/quick-validate (poly/fn _not-id [a] 1)
                                          {:schema (poly/all [X] (poly/=> X X))}))))
  (is (true? (:pass? (sut/quick-validate
                       (poly/fn :- s/Int
                         [a :- s/Int] a)))))
  ;; (=> (not (Int "0")) Int)
  (is (false? (:pass? (sut/quick-validate
                        (poly/fn :- s/Int
                          [a :- s/Int] (str a))))))
  (is (= '(=> (not (integer? "0"))
              (integer? 0))
         (sut/check
           (poly/fn :- s/Int
             [a :- s/Int] (str a)))))
  (is (nil? (sut/check
              (poly/fn; :- (s/=> s/Int s/Int)
                [a :- (s/=> s/Int s/Int)] a))))
  (is (nil? (sut/check
              (poly/fn :- (poly/=> s/Int s/Int)
                [a :- (poly/=> s/Int s/Int)] a))))
  (is (= '(=> (=> (not (integer? false)) Int)
              (=> Bool Int))
         (sut/check
           (poly/fn :- (poly/=> s/Int s/Int)
             [a :- (poly/=> s/Bool s/Int)] a))))
  (is (= '(=> (=> (not (boolean? 0)) (integer? 0))
              (=> Int Int))
         (sut/check
           (poly/fn :- (poly/=> s/Bool s/Int)
             [a :- (poly/=> s/Int s/Int)] a))))
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

(def every-pred-true-schema
  "Tests that every-pred returns true if passes all preds."
  (all [X]
       (=> (=> (s/eq true) & [X])
           (=> poly/AnyTrue X)
           & [(=> poly/AnyTrue X)])))

(deftest every-pred-true-schema-test
  (is (:pass? (sut/quick-validate
                every-pred
                {:schema (poly/instantiate every-pred-true-schema s/Int)
                 :num-tests 20})))
  (is (:pass? (sut/quick-validate
                every-pred
                {:schema every-pred-true-schema
                 :num-tests 20})))
  (is (false? (:pass? (sut/quick-validate
                        (complement every-pred)
                        {:schema every-pred-true-schema})))))

(def every-pred-short-one-arg-schema
  "Tests that every-pred short-circuits on the first failed predicate when passed one arg."
  (all [X Y :.. Z :..]
       (s/make-fn-schema 
         (=> (s/eq false) X)
         [(-> (mapv (fn [_] (s/one (=> poly/AnyTrue X) (gensym))) Y)
              (conj (s/one (=> poly/AnyFalse X) (gensym)))
              (into (map (fn [_] (s/one (=> s/Bool poly/NeverInput) (gensym)))) Z))])))

#_ ;;TODO imaginary syntax for above
(all [X Y :.. Z :..]
     (=> (=> (s/eq false) X)
         (=> poly/AnyTrue X) :.. Y
         (=> poly/AnyFalse X)
         (=> s/Bool poly/NeverInput) :.. Z))

(deftest every-pred-short-one-arg-test
  (testing "schema expands as expected"
    (is (= '(=> (=> (eq false) Int)
                (=> (enum nil false) Int))
           (s/explain (poly/instantiate every-pred-short-one-arg-schema s/Int [] []))))
    (is (= '(=> (=> (eq false) Int)
                (=> (pred AnyTrue) Int) ;; pass
                (=> (enum nil false) Int) ;; fail
                (=> Bool NeverInput)) ;; short-circuit
           (s/explain (poly/instantiate every-pred-short-one-arg-schema s/Int [1] [2]))))
    (is (= '(=> (=> (eq false) Int)
                (=> (pred AnyTrue) Int) ;; pass
                (=> (pred AnyTrue) Int) ;; pass
                (=> (enum nil false) Int) ;; fail
                (=> Bool NeverInput) ;;short-circuit
                (=> Bool NeverInput)) ;;short-circuit
           (s/explain (poly/instantiate every-pred-short-one-arg-schema s/Int [1 1] [2 2])))))
  (is (:pass? (sut/quick-validate
                every-pred
                {:schema (poly/instantiate every-pred-short-one-arg-schema
                                           s/Int [] [])})))
  (is (:pass? (sut/quick-validate
                every-pred
                {:schema (poly/instantiate every-pred-short-one-arg-schema
                                           s/Int [s/Any] [])})))
  (is (:pass? (sut/quick-validate
                every-pred
                {:schema (poly/instantiate every-pred-short-one-arg-schema
                                           s/Int [s/Any s/Any s/Any] [])})))
  (is (:pass? (sut/quick-validate every-pred {:schema every-pred-short-one-arg-schema
                                              :num-tests 30})))
  ;; TODO push in the "not" to show problem
  #_(=> (=> (s/eq false) (s/eq X))
        (=> AnyFalse (s/eq X))
        (=> Bool (not (NeverInput X))))
  (is (= '(not (=> (=> (eq false) (eq X))
                   (=> (enum nil false) (eq X))
                   (=> Bool NeverInput)))
         (sut/check (fn [& fs]
                      (fn [& args]
                        (doseq [f fs
                                arg args]
                          (f arg))
                        (apply (apply every-pred fs) args)))
                    {:schema every-pred-short-one-arg-schema}))))

#_ ;;TODO every-pred schema for multiple args. needs fn intersections to generate
   ;; prediate that only passes some schemas.
(def every-pred-short-nargs-schema
  "Tests that every-pred short-circuits on the first failed predicate when passed one arg."
  (all [GOOD_ARGS :.. BAD0 BAD_ARGS :.. BEFORE_FAIL_PREDS :.. AFTER_FAIL_PREDS :..]
       (let [BAD_ARGS (conj BAD_ARGS BAD0)]
         (s/make-fn-schema 
           (=> (s/eq false) GOOD_ARGS :.. GOOD_ARGS)
           [(-> (mapv (fn [_] (s/one (=> poly/AnyTrue X) (gensym))) BEFORE_FAIL_PREDS)
                (conj (s/one (=> poly/AnyFalse X) (gensym)))
                (into (map (fn [_] (s/one (=> s/Bool (apply s/cond-pre (concat GOOD_ARGS BAD_ARGS))) (gensym))))
                      AFTER_FAIL_PREDS))]))))

(deftest nested-poly-test
  (is (:pass? (sut/quick-validate identity {:schema (all [X] (=> X X))})))
  (is (not (:pass? (sut/quick-validate + {:schema (all [X] (=> X X))}))))
  ;; TODO (not (=> (throws? (eq X)) (eq X)))
  (is (= '(not (=> (eq X) (eq X)))
         (sut/check + {:schema (all [X] (=> X X))})))
  (is (:pass? (sut/quick-validate (fn [] identity) {:schema (=> (all [X] (=> X X)))})))
  #_ ;;FIXME
  (is (not (:pass? (sut/quick-validate (fn [] +) {:schema (=> (all [X] (=> X X)))})))))

;; TODO play with (=> (all [X] (=> X X))). 
(def comp-zero-arg-schema
  "All valid ways of calling comp and using its result."
  (all [X] (=> (=> X X))))

(deftest comp-zero-arg-schema-test
  (is (:pass? (sut/quick-validate
                comp
                {:schema comp-zero-arg-schema})))
  (is (false? (:pass? (sut/quick-validate
                        (complement comp)
                        {:schema comp-zero-arg-schema}))))
  (is (false? (:pass? (sut/quick-validate
                        (fn [] +)
                        {:schema comp-zero-arg-schema})))))

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
           (s/explain (poly/instantiate comp-schema [(s/eq :Y0) (s/eq :Y1)] [(s/eq :X0) (s/eq :X1) (s/eq :X2)] (s/eq :X3))))))
  (is (:pass? (sut/quick-validate
                comp
                {:schema (poly/instantiate comp-schema [] [] [(s/eq :X)])})))
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
                 :num-tests 30})))
  (is (false? (:pass? (sut/quick-validate
                        every-pred
                        {:schema comp-schema
                         :num-tests 30}))))
  (is (false? (:pass? (sut/quick-validate
                        (fn _reversed-comp [& args]
                          (apply comp (reverse args)))
                        {:schema comp-schema})))))

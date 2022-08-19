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
          res (sut/validate
                (poly/fn [a :- s/Int] (swap! atm conj a) a)
                {:num-tests 2
                 :seed 1634677540521})]
      (is (= {:result true, :pass? true, :num-tests 2, :seed 1634677540521}
             (dissoc res :time-elapsed-ms)))
      (is (= [-1N 0] @atm))))
  (testing "failure"
    (let [res (sut/validate
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
  (is (:pass? (sut/validate (poly/fn :all [X] _id :- X [a :- X] a))))
  (is (false? (:pass? (sut/validate (poly/fn :all [X] _not-id :- X [a :- X] 1)))))
  (is (false? (:pass? (sut/validate (poly/fn _not-id [a] 1)
                                 {:schema (poly/all [X] (poly/=> X X))}))))
  (is (:pass? (sut/validate
                (poly/fn :- s/Int
                  [a :- s/Int] a))))
  (is (false? (:pass? (sut/validate
                        (poly/fn :- s/Int
                          [a :- s/Int] (str a))))))
  (is (:pass? (sut/validate
                (poly/fn; :- (s/=> s/Int s/Int)
                  [a :- (s/=> s/Int s/Int)] a))))
  (is (:pass? (sut/validate
                (poly/fn :- (poly/=> s/Int s/Int)
                  [a :- (poly/=> s/Int s/Int)] a))))
  #_ ;;TODO
  (sut/validate
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
         & [(=> s/Any X)]
         ))
#_
(all [X Y :.. Z :..]
     (s/make-fn-schema 
       (=> (s/eq false) X)
       [(->> (mapv #(s/one (=> poly/AnyTrue %) (gensym)) Y)
             (conj (s/one (=> poly/AnyFalse X) (gensym)))
             (into (mapv (fn [_] (s/one (=> s/Bool poly/Never) (gensym))) Z)))]))

(def every-pred-short-circuits-schema
  (all [X Y :.. Z :..]
       (s/make-fn-schema 
         (=> (s/eq false) X)
         [(-> (mapv (fn [_] (s/one (=> poly/AnyTrue X) (gensym))) Y)
              (conj (s/one (=> poly/AnyFalse X) (gensym)))
              (into (mapv (fn [_] (s/one (=> s/Bool poly/Never) (gensym))) Z)))])))

(deftest Never-test
  (is (s/validate poly/Never false)))

(deftest every-pred-short-circuits-test
  (is (= '(=> (=> (eq false) Int) (=> (enum nil false) Int))
         (s/explain (poly/instantiate every-pred-short-circuits-schema s/Int [] []))))
  (is (= '(=> (=> (eq false) Int) (=> (pred AnyTrue) Int) (=> (enum nil false) Int) (=> Bool (pred Never)))
         (s/explain (poly/instantiate every-pred-short-circuits-schema s/Int [1] [2]))))
  (is (:pass? (sut/validate every-pred {:schema every-pred-short-circuits-schema})))
  ;; TODO FnSchema's don't generatively test while validating.
  #_
  (is (not (:pass? (sut/validate (fn [& fs]
                                   (fn [& args]
                                     (doseq [f fs
                                             arg args]
                                       (f arg))
                                     (apply (apply every-pred fs) args)))
                                 {:schema every-pred-short-circuits-schema})))))

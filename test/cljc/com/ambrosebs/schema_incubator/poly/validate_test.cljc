(ns com.ambrosebs.schema-incubator.poly.validate-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [com.ambrosebs.schema-incubator.poly :as poly :refer [=> all]]
            [com.ambrosebs.schema-incubator.poly.check :as sut]
            [schema-generators.generators :as sgen]
            [schema.core :as s]
            [schema.utils :as utils]))

(deftest check-test
  (testing "success"
    (let [atm (atom [])
          res (sut/check
                (poly/fn [a :- s/Int] (swap! atm conj a) a)
                {:num-tests 2
                 :seed 1634677540521})]
      (is (= {:result true, :pass? true, :num-tests 2, :seed 1634677540521}
             (dissoc res :time-elapsed-ms)))
      (is (= [-1N 0] @atm))))
  (testing "failure"
    (let [res (sut/check
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

(deftest poly-check-test
  (is (:pass? (sut/check (poly/fn :all [X] _id :- X [a :- X] a))))
  (is (false? (:pass? (sut/check (poly/fn :all [X] _not-id :- X [a :- X] 1)))))
  (is (false? (:pass? (sut/check (poly/fn _not-id [a] 1)
                                 {:schema (poly/all [X] (poly/=> X X))}))))
  (is (:pass? (sut/check
                (poly/fn :- s/Int
                  [a :- s/Int] a))))
  (is (false? (:pass? (sut/check
                        (poly/fn :- s/Int
                          [a :- s/Int] (str a))))))
  (is (:pass? (sut/check
                (poly/fn; :- (s/=> s/Int s/Int)
                  [a :- (s/=> s/Int s/Int)] a))))
  (is (:pass? (sut/check
                (poly/fn :- (poly/=> s/Int s/Int)
                  [a :- (poly/=> s/Int s/Int)] a))))
  #_ ;;TODO
  (sut/check
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
  (is (s/check poly/Never false)))

(deftest every-pred-short-circuits-test
  (is (= '(=> (=> (eq false) Int) (=> (enum nil false) Int))
         (s/explain (poly/instantiate every-pred-short-circuits-schema s/Int [] []))))
  (is (= '(=> (=> (eq false) Int) (=> (pred AnyTrue) Int) (=> (enum nil false) Int) (=> Bool (pred Never)))
         (s/explain (poly/instantiate every-pred-short-circuits-schema s/Int [1] [2]))))
  (is (:pass? (sut/check every-pred {:schema every-pred-short-circuits-schema})))
  ;; TODO FnSchema's don't generatively test while validating.
  #_
  (is (not (:pass? (sut/check (fn [& fs]
                                (fn [& args]
                                  (doseq [f fs
                                          arg args]
                                    (f arg))
                                  (apply (apply every-pred fs) args)))
                              {:schema every-pred-short-circuits-schema})))))

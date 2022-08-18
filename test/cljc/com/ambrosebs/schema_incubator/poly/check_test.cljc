(ns com.ambrosebs.schema-incubator.poly.check-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [com.ambrosebs.schema-incubator.poly :as poly :refer [=> all]]
            [com.ambrosebs.schema-incubator.poly.check :as sut]
            [schema-generators.generators :as sgen]
            [schema.core :as s]))

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
  (is (:pass? (sut/check (poly/fn :all [X] :- X [a :- X] a))))
  (is (not (:pass? (sut/check (poly/fn :all [X] :- X [a :- X] 1)))))
  (is (not (:pass? (sut/check (poly/fn [a] 1)
                              {:schema (poly/all [X] (poly/=> X X))}))))
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

#_
(all [X :.. Y :- [:.. :min 1]]
     (=> (=> (poly/app... [Y] (peek Y)) X :.. X)
         (=> (poly/app... [Y] (second Y)) (poly/app... [Y] (first Y))) :.. [Y] (partition-all 2 Y 1)
         (=> (poly/app... [Y] (first Y)) X :.. X)))

(ns com.ambrosebs.schema-incubator.poly-test
  ;schema.core-test
  "Tests for schema.

   Uses helpers defined in schema.test-macros (for cljs sake):
    - (valid! s x) asserts that (s/check s x) returns nil
    - (invalid! s x) asserts that (s/check s x) returns a validation failure
      - The optional last argument also checks the printed Clojure representation of the error.
    - (invalid-call! s x) asserts that calling the function throws an error."
  (:refer-clojure :exclude [parse-long])
  #?(:clj (:use clojure.test [com.ambrosebs.schema-incubator.poly.test-macros :only [valid! invalid! invalid-call! is-assert!]]))
  #?(:cljs (:use-macros
             [cljs.test :only [is deftest testing are]]
             [com.ambrosebs.schema-incubator.poly.test-macros :only [valid! invalid! invalid-call! is-assert!]]))
  #?(:cljs (:require-macros [clojure.template :refer [do-template]]
                            [schema.macros :as macros]))
  (:require
   [clojure.string :as str]
   [#?(:clj clojure.pprint
       :cljs cljs.pprint) :as pprint]
   #?(:clj [clojure.template :refer [do-template]])
   clojure.data
   [schema.utils :as utils]
   [schema.core :as s]
   [com.ambrosebs.schema-incubator.poly :as poly]
   [com.ambrosebs.schema-incubator.poly.other-namespace :as other-namespace]
   [schema.spec.core :as spec]
   [schema.spec.collection :as collection]
   #?(:clj [schema.macros :as macros])
   #?(:cljs cljs.test)))

#?(:cljs
(do
  (def Exception js/Error)
  (def AssertionError js/Error)
  (def Throwable js/Error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s/protocol

(defprotocol ATestMarkerProtocol)

(poly/defn ^:always-validate a-test-marker-protocol-fn
  "Compile the schema before extending, make sure it works as expected"
  [x :- (s/protocol ATestMarkerProtocol)]
  x)

(defrecord DirectTestMarkerProtocolSatisfier [] ATestMarkerProtocol)
(defrecord IndirectTestMarkerProtocolSatisfier []) (extend-type IndirectTestMarkerProtocolSatisfier ATestMarkerProtocol)
(defrecord NonTestMarkerProtocolSatisfier [])

(deftest marker-protocol-test
 (let [schema (s/protocol ATestMarkerProtocol)]
   (valid! schema (DirectTestMarkerProtocolSatisfier.))
   (valid! schema (IndirectTestMarkerProtocolSatisfier.))
   (invalid! schema (NonTestMarkerProtocolSatisfier.))
   (invalid! schema nil)
   (invalid! schema 117 "(not (satisfies? ATestMarkerProtocol 117))")
   (is (a-test-marker-protocol-fn (DirectTestMarkerProtocolSatisfier.)))
   (is (a-test-marker-protocol-fn (IndirectTestMarkerProtocolSatisfier.)))
   (invalid-call! a-test-marker-protocol-fn (NonTestMarkerProtocolSatisfier.))
   (is (= '(protocol ATestMarkerProtocol) (s/explain schema)))))

(defprotocol ATestProtocol
  (not-marker-protocol [this]))

(poly/defn ^:always-validate a-test-non-marker-protocol-fn
  "Compile the schema before extending, make sure it works as expected"
  [x :- (s/protocol ATestProtocol)]
  x)

(defrecord DirectTestProtocolSatisfier [] ATestProtocol (not-marker-protocol [this]))
(defrecord IndirectTestProtocolSatisfier []) (extend-type IndirectTestProtocolSatisfier ATestProtocol (not-marker-protocol [this]))
(defrecord NonTestProtocolSatisfier [])

(deftest non-marker-protocol-test
  (let [schema (s/protocol ATestProtocol)]
    (valid! schema (DirectTestProtocolSatisfier.))
    (valid! schema (IndirectTestProtocolSatisfier.))
    (invalid! schema (NonTestProtocolSatisfier.))
    (invalid! schema nil)
    (invalid! schema 117 "(not (satisfies? ATestProtocol 117))")
    (is (a-test-non-marker-protocol-fn (DirectTestProtocolSatisfier.)))
    (is (a-test-non-marker-protocol-fn (IndirectTestProtocolSatisfier.)))
    (invalid-call! a-test-non-marker-protocol-fn (NonTestProtocolSatisfier.))
    (is (= '(protocol ATestProtocol) (s/explain schema)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Record Schemas

(defrecord Foo [x y])

(deftest record-test
  (let [schema (s/record Foo {:x s/Any (s/optional-key :y) s/Int})]
    (valid! schema (Foo. :foo 1))
    (invalid! schema {:x :foo :y 1})
    (invalid! schema (assoc (Foo. :foo 1) :bar 2))
    #?(:clj (is (= '(record com.ambrosebs.schema_incubator.poly_test.Foo {:x Any, (optional-key :y) Int})
                   (s/explain schema))))))

(deftest record-with-extra-keys-test
  (let [schema (s/record Foo {:x s/Any
                              :y s/Int
                              s/Keyword s/Any})]
    (valid! schema (Foo. :foo 1))
    (valid! schema (assoc (Foo. :foo 1) :bar 2))
    (invalid! schema {:x :foo :y 1})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function Schemas

(deftest single-arity-fn-schema-test
  (let [schema (poly/=> s/Keyword s/Int s/Int)]
    (valid! schema (fn [x y] (keyword (str (+ x y)))))
    (valid! schema (fn [])) ;; we don't actually validate what the function does
    (valid! schema {})
    (is (= '(=> Keyword Int Int) (s/explain schema)))))

(deftest single-arity-and-more-fn-schema-test
  (let [schema (poly/=> s/Keyword s/Int s/Int & [s/Keyword])]
    (valid! schema (fn [])) ;; we don't actually validate what the function does
    (valid! schema {})
    (is (= '(=> Keyword Int Int & [Keyword]) (s/explain schema)))))

(deftest multi-arity-fn-schema-test
  (let [schema (poly/=>* s/Keyword [s/Int] [s/Int & [s/Keyword]])]
    (valid! schema (fn [])) ;; we don't actually validate what the function does
    (valid! schema {})
    (is (= '(=>* Keyword [Int] [Int & [Keyword]]) (s/explain schema)))))

(deftest dotted-fn-schema-test
  (testing "expand dotted template"
    (let [X [s/Int s/Bool]
          schema (poly/=> s/Keyword s/Int s/Int [X] :.. X)]
      (is (= (poly/=> s/Keyword s/Int s/Int [s/Int] [s/Bool])
             schema))))
  (testing "expand AnyDotted"
    (let [X (poly/->AnyDotted s/Int)
          schema (poly/=> s/Keyword s/Int s/Int [X] :.. X)]
      (is (= (poly/=> s/Keyword s/Int s/Int & [[s/Int]])
             schema)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schematized defrecord

#?(:clj
(defmacro test-normalized-meta [symbol ex-schema desired-meta]
  (let [normalized (macros/normalized-metadata &env symbol ex-schema)]
    `(do (is (= '~symbol '~normalized))
         (is (= ~(select-keys desired-meta [:schema :tag])
                ~(select-keys (meta normalized) [:schema :tag])))))))

#?(:clj
(do
  (def ASchema [long])

  (deftest normalized-metadata-test
    (testing "empty" (test-normalized-meta 'foo nil {:schema s/Any}))
    (testing "primitive" (test-normalized-meta ^long foo nil {:tag long :schema long}))
    (testing "class" (test-normalized-meta ^String foo nil {:tag String :schema String}))
    (testing "non-tag" (test-normalized-meta ^ASchema foo nil {:schema ASchema}))
    (testing "explicit" (test-normalized-meta ^Object foo String {:tag Object :schema String}))
    (testing "xplatform" (test-normalized-meta ^s/Str foo nil {:tag String :schema String})))

  (defmacro test-meta-extraction [meta-form arrow-form]
    (let [meta-ized (macros/process-arrow-schematized-args {} arrow-form)]
      `(do (is (= '~meta-form '~meta-ized))
           (is (= ~(mapv #(select-keys (meta (macros/normalized-metadata {} % nil)) [:schema :tag]) meta-form)
                  ~(mapv #(select-keys (meta %) [:schema :tag]) meta-ized))))))

  (deftest extract-arrow-schematized-args-test
    (testing "empty" (test-meta-extraction [] []))
    (testing "no-tag" (test-meta-extraction [x] [x]))
    (testing "old-tags" (test-meta-extraction [^String x] [^String x]))
    (testing "new-vs-old-tag" (test-meta-extraction [^String x] [x :- String]))
    (testing "xplatform-new-vs-xplatform-old-tag" (test-meta-extraction [^s/Str x] [x :- s/Str]))
    (testing "multi vars" (test-meta-extraction [x ^String y z] [x y :- String z])))))

(defprotocol PProtocol
  (do-something [this]))

;; exercies some different arities

(s/defrecord Bar
    [^s/Int foo ^s/Str bar]
  {(s/optional-key :baz) s/Keyword})

(s/defrecord Bar2
    [^s/Int foo ^s/Str bar]
  {(s/optional-key :baz) s/Keyword}
  PProtocol
  (do-something [this] 2))

(s/defrecord Bar3
    [^s/Int foo ^s/Str bar]
  PProtocol
  (do-something [this] 3))

(s/defrecord Bar4
    [foo :- [s/Int]
     bar :- (s/maybe {s/Str s/Str})]
  PProtocol
  (do-something [this] 4))

(deftest defrecord-schema-test
  (is (= (utils/class-schema Bar)
         (s/record Bar {:foo s/Int
                        :bar s/Str
                        (s/optional-key :baz) s/Keyword})))
  (is (identity (Bar. 1 :foo)))
  (is (= #{:foo :bar} (set (keys (map->Bar {:foo 1})))))
  ;; (is (thrown? Exception (map->Bar {}))) ;; check for primitive long
  (valid! Bar (Bar. 1 "test"))
  (invalid! Bar (Bar. 1 :foo))
  (valid! Bar (assoc (Bar. 1 "test") :baz :foo))
  (invalid! Bar (assoc (Bar. 1 "test") :baaaz :foo))
  (invalid! Bar (assoc (Bar. 1 "test") :baz "foo"))

  (valid! Bar2 (assoc (Bar2. 1 "test") :baz :foo))
  (invalid! Bar2 (assoc (Bar2. 1 "test") :baaaaz :foo))
  (is (= 2 (do-something (Bar2. 1 "test"))))

  (valid! Bar3 (Bar3. 1 "test"))
  (invalid! Bar3 (assoc (Bar3. 1 "test") :foo :bar))
  (is (= 3 (do-something (Bar3. 1 "test"))))

  (valid! Bar4 (Bar4. [1] {"test" "test"}))
  (valid! Bar4 (Bar4. [1] nil))
  (invalid! Bar4 (Bar4. ["a"] {"test" "test"}))
  (is (= 4 (do-something (Bar4. 1 "test")))))

(s/defrecord BarNewStyle
    [foo :- s/Int
     bar :- s/Str
     zoo]
  {(s/optional-key :baz) s/Keyword})

(deftest defrecord-new-style-schema-test
  (is (= (utils/class-schema BarNewStyle)
         (s/record BarNewStyle {:foo s/Int
                                :bar s/Str
                                :zoo s/Any
                                (s/optional-key :baz) s/Keyword})))
  (is (identity (BarNewStyle. 1 :foo "a")))
  (is (= #{:foo :bar :zoo} (set (keys (map->BarNewStyle {:foo 1})))))
  ;; (is (thrown? Exception (map->BarNewStyle {}))) ;; check for primitive long
  (valid! BarNewStyle (BarNewStyle. 1 "test" "a"))
  (invalid! BarNewStyle (BarNewStyle. 1 :foo "a"))
  (valid! BarNewStyle (assoc (BarNewStyle. 1 "test" "a") :baz :foo))
  (invalid! BarNewStyle (assoc (BarNewStyle. 1 "test" "a") :baaaz :foo))
  (invalid! BarNewStyle (assoc (BarNewStyle. 1 "test" "a") :baz "foo")))


;; Now test that schemata and protocols work as type hints.
;; (auto-detecting protocols only works in clj currently)

(def LongOrString (s/either s/Int s/Str))

#?(:clj (s/defrecord Nested [^Bar4 b ^LongOrString c p :- (s/protocol PProtocol)]))
(s/defrecord NestedExplicit [b :- Bar4 c :- LongOrString p :- (s/protocol PProtocol)])

(defn test-fancier-defrecord-schema [klass konstructor]
  (let [bar1 (Bar. 1 "a")
        bar2 (Bar2. 1 "a")]
    (is (= (utils/class-schema klass)
           (s/record
            klass
            {:b Bar4
             :c LongOrString
             :p (s/protocol PProtocol)}
            konstructor)))
    (valid! klass (konstructor {:b (Bar4. [1] {}) :c 1 :p bar2}))
    (valid! klass (konstructor {:b (Bar4. [1] {}) :c "hi" :p bar2}))
    (invalid! klass (konstructor {:b (Bar4. [1] {}) :c "hi" :p bar1}))
    (invalid! klass (konstructor {:b (Bar4. [1] {:foo :bar}) :c 1 :p bar2}))
    (invalid! klass (konstructor {:b nil :c "hi" :p bar2}))))

(deftest fancier-defrecord-schema-test
  #?(:clj (test-fancier-defrecord-schema Nested map->Nested))
  (test-fancier-defrecord-schema NestedExplicit map->NestedExplicit))


(s/defrecord OddSum
    [a b]
  {}
  #(odd? (+ (:a %) (:b %))))

(deftest defrecord-extra-validation-test
  (valid! OddSum (OddSum. 1 2))
  (invalid! OddSum (OddSum. 1 3)))

#?(:clj
(do (s/defrecord RecordWithPrimitive [x :- long])
    (deftest record-with-primitive-test
      (valid! RecordWithPrimitive (RecordWithPrimitive. 1))
      #?(:bb (do (is (invalid! RecordWithPrimitive (RecordWithPrimitive. "a")))
                 (is (invalid! RecordWithPrimitive (RecordWithPrimitive. nil))))
         :default (do (is (thrown? Exception (RecordWithPrimitive. "a")))
                      (is (thrown? Exception (RecordWithPrimitive. nil))))))))

(deftest map->record-test
  (let [subset {:foo 1 :bar "a"}
        exact (assoc subset :zoo :zoo)
        superset (assoc exact :baz :baz)]
    (testing "map->record"
      (is (= (assoc subset :zoo nil)
             (into {} (map->BarNewStyle subset))))
      (is (= exact
             (into {} (map->BarNewStyle exact))))
      (is (= superset
             (into {} (map->BarNewStyle superset)))))

    (testing "strict-map->record"
      (is (thrown? Exception (strict-map->BarNewStyle subset)))
      (is (= exact (into {} (strict-map->BarNewStyle exact))))
      (is (= exact (into {} (strict-map->BarNewStyle exact true))))
      (is (thrown? Exception (strict-map->BarNewStyle superset)))
      (is (= exact (into {} (strict-map->BarNewStyle superset true)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schematized functions

#?(:clj
(deftest split-rest-arg-test
  (is (= (macros/split-rest-arg {} ['a '& 'b])
         '[[a] b]))
  (is (= (macros/split-rest-arg {} ['a 'b])
         '[[a b] nil]))))

;;; fn

(def OddLong (s/both (s/pred odd?) #?(:cljs s/Int :clj long)))

(def +test-fn-schema+
  "Schema for (poly/fn ^String [^OddLong x y])"
  (poly/=> s/Str OddLong s/Any))

(deftest simple-validated-meta-test
  (let [f (poly/fn ^s/Str foo [^OddLong arg0 arg1])]
    (is (= +test-fn-schema+ (s/fn-schema f)))))

#?(:bb nil :clj
(deftest no-wrapper-fn-test
  (let [f (poly/fn this [] this)]
    (is (identical? f (f))))))

(deftest no-schema-fn-test
  (let [f (poly/fn [arg0 arg1] (+ arg0 arg1))]
    (is (= (poly/=> s/Any s/Any s/Any) (s/fn-schema f)))
    (s/with-fn-validation
      (is (= 4 (f 1 3))))
    (is (= 4 (f 1 3)))))

(deftest simple-validated-fn-test
  (let [fthiss (atom [])
        f (poly/fn test-fn :- (s/pred even?)
            [^s/Int x y :- {:foo (s/both s/Int (s/pred odd?))}]
            (swap! fthiss conj test-fn)
            (+ x (:foo y -100)))]
    (s/with-fn-validation
      (is (= 4 (f 1 {:foo 3})))
      ;; Primitive Interface Test
      #?(:bb nil
         :clj (is (thrown? Exception (.invokePrim ^clojure.lang.IFn$LOL f 1 {:foo 3})))) ;; primitive type hints don't work on fns
      (invalid-call! f 1 {:foo 4})  ;; foo not odd?
      (invalid-call! f 2 {:foo 3})) ;; return not even?

    (is (= 5 (f 1 {:foo 4})))     ;; foo not odd?
    (is (= 4.0 (f 1.0 {:foo 3}))) ;; first arg not long
    (is (= 5 (f 2 {:foo 3})))     ;; return not even?
    (let [fthiss @fthiss]
      (is (seq fthiss))
      #?(:bb nil
         :clj (is (every? #(identical? % f) fthiss)))))
  (testing
    "Tests that the anonymous function schema macro can handle a
    name, a schema without a name and no return schema."
    (let [named-square (poly/fn square :- s/Int [x :- s/Int]
                         (* x x))
          anon-square (poly/fn :- s/Int [x :- s/Int]
                        (* x x))
          arg-only-square (poly/fn [x :- s/Int] (* x x))]
      (is (= 100
             (named-square 10)
             (anon-square 10)
             (arg-only-square 10))))))



(deftest always-validated-fn-test
  (let [f (poly/fn ^:always-validate test-fn :- (s/pred even?)
            [x :- (s/pred pos?)]
            (inc x))]
    (is (= 2 (f 1)))
    (invalid-call! f 2)
    (invalid-call! f -1)))


(poly/defn ^:never-validate never-validated-test-fn :- (s/pred even?)
  [x :- (s/pred pos?)]
  (inc x))

(deftest never-validated-fn-test
  (doseq [f [never-validated-test-fn
             (poly/fn ^:never-validate test-fn :- (s/pred even?)
               [x :- (s/pred pos?)]
               (inc x))]]
    (s/with-fn-validation
      (is (= 2 (f 1)))
      (is (= 3 (f 2)))
      (is (= 0 (f -1))))))

(poly/defn ^:never-validate never-validated-rest-test-fn :- (s/pred even?)
  [arg0 & [rest0 :- (s/pred pos?)]]
  (+ arg0 (or rest0 2)))

(deftest never-validated-rest-test
  (doseq [f [never-validated-rest-test-fn
             (poly/fn ^:never-validate rest-test-fn :- (s/pred even?)
               [arg0 & [rest0 :- (s/pred pos?)]]
               (+ arg0 (or rest0 2)))]]
    (s/with-fn-validation
      (is (= 2 (f 0)))
      (is (= 4 (f 2 2)))
      (is (= 1 (f 2 -1))))))

(s/set-compile-fn-validation! false)

(poly/defn elided-validation-test-fn :- (s/pred even?)
  [x :- (s/pred pos?)]
  (inc x))

(poly/defn ^:always-validate elided-validation-always-test-fn :- (s/pred even?)
  [x :- (s/pred pos?)]
  (inc x))

(s/set-compile-fn-validation! true)

(deftest elided-validation-test
  (doseq [f [elided-validation-test-fn
             elided-validation-always-test-fn]]
    (s/with-fn-validation
      (is (= 2 (f 1)))
      (is (= 3 (f 2)))
      (is (= 0 (f -1))))))

(defn parse-long [x]
  #?(:clj (Long/parseLong x)
     :cljs (js/parseInt x)))

(deftest destructured-validated-fn-test
  (let [LongPair [(s/one s/Int 'x) (s/one s/Int 'y)]
        f (poly/fn foo :- s/Int
            [^LongPair [x y] ^s/Int arg1]
            (+ x y arg1))]
    (is (= (poly/=> s/Int LongPair s/Int)
           (s/fn-schema f)))
    (s/with-fn-validation
      (is (= 6 (f [1 2] 3)))
      (invalid-call! f ["a" 2] 3))))

(deftest two-arity-fn-test
  (let [f (poly/fn foo :- s/Int
            ([^s/Str arg0 ^s/Int arg1] (+ arg1 (foo arg0)))
            ([^s/Str arg0] (parse-long arg0)))]
    (is (= (poly/=>* s/Int [s/Str] [s/Str s/Int])
           (s/fn-schema f)))
    (is (= 3 (f "3")))
    (is (= 10 (f "3" 7)))))

(deftest infinite-arity-fn-test
  (let [f (poly/fn foo :- s/Int
            ([^s/Int arg0] (inc arg0))
            ([^s/Int arg0 & strs :- [s/Str]]
               (reduce + (foo arg0) (map count strs))))]
    (is (= (poly/=>* s/Int [s/Int] [s/Int & [s/Str]])
           (s/fn-schema f)))
    (s/with-fn-validation
      (is (= 5 (f 4)))
      (is (= 16 (f 4 "55555" "666666")))
      (invalid-call! f 4 [3 3 3]))))

(deftest rest-arg-destructuring-test
  (testing "no schema"
    (let [fthiss (atom [])
          f (poly/fn foo :- s/Int
              [^s/Int arg0 & [rest0]]
              (swap! fthiss conj foo)
              (+ arg0 (or rest0 2)))]
      (is (= (poly/=>* s/Int [s/Int & [(s/optional s/Any 'rest0)]])
             (s/fn-schema f)))
      (s/with-fn-validation
        (is (= 6 (f 4)))
        (is (= 9 (f 4 5)))
        (invalid-call! f 4 9 2))
      (let [fthiss @fthiss]
        (is (seq fthiss))
        #?(:bb nil
           :clj (is (every? #(identical? % f) fthiss))))))
  (testing "arg schema"
    (let [f (poly/fn foo :- s/Int
              [^s/Int arg0 & [rest0 :- s/Int]] (+ arg0 (or rest0 2)))]
      (is (= (poly/=>* s/Int [s/Int & [(s/optional s/Int 'rest0)]])
             (s/fn-schema f)))
      (s/with-fn-validation
        (is (= 6 (f 4)))
        (is (= 9 (f 4 5)))
        (invalid-call! f 4 9 2)
        (invalid-call! f 4 1.5))))
  (testing "list schema"
    (let [f (poly/fn foo :- s/Int
              [^s/Int arg0 & [rest0] :- [s/Int]] (+ arg0 (or rest0 2)))]
      (is (= (poly/=>* s/Int [s/Int & [s/Int]])
             (s/fn-schema f)))
      (s/with-fn-validation
        (is (= 6 (f 4)))
        (is (= 9 (f 4 5)))
        (is (= 9 (f 4 5 9)))
        (invalid-call! f 4 1.5)))))

(deftest fn-recursion-test
  (testing "non-tail recursion"
    (let [f (poly/fn fib :- s/Int [n :- s/Int]
              (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))]
      (is (= 8 (f 6)))
      (s/with-fn-validation
        (is (= 8 (f 6))))))
  (testing "tail recursion"
    (let [f (poly/fn fact :- s/Int [n :- s/Int ret :- s/Int]
              (if (<= n 1) ret (recur (dec n) (* ret n))))]
      (is (= 120 (f 5 1)))
      (s/with-fn-validation
        (is (= 120 (f 5 1)))))))

(deftest fn-metadata-test
  (let [->mkeys #(set (keys (meta %)))]
    (is (= (into (->mkeys (poly/fn [])) [:blah])
           (->mkeys ^:blah (poly/fn []))))))

;;; defn

(def OddLongString
  (s/both s/Str (s/pred #(odd? (parse-long %)) 'odd-str?)))

(poly/defn ^{:tag String} simple-validated-defn :- OddLongString
  "I am a simple schema fn"
  {:metadata :bla}
  [arg0 :- OddLong]
  (str arg0))

(poly/defn validated-pre-post-defn :- OddLong
  "I have pre/post conditions"
  [arg0 :- s/Num]
  {:pre  [(odd? arg0) (> 10 arg0)]
   :post [(odd? %)    (<  5 %)]}
  arg0)

(def +simple-validated-defn-schema+
  (poly/=> OddLongString OddLong))

(def ^String +bad-input-str+ "Input to simple-validated-defn does not match schema")

;; Test that poly/defn returns var
#?(:clj
(with-test
  (poly/defn with-test-fn [a b] (+ a b))
  (is (= 3 (with-test-fn 1 2)))
  (is (= 0 (with-test-fn 10 -10)))))

#?(:cljs
(deftest simple-validated-defn-test
  (s/with-fn-validation
    (is (= "3" (simple-validated-defn 3)))
    (invalid-call! simple-validated-defn 4)
    (invalid-call! simple-validated-defn "a"))
  (s/with-fn-validation
    (is (= 7 (validated-pre-post-defn 7)))
    (invalid-call! validated-pre-post-defn 0)
    (invalid-call! validated-pre-post-defn 11)
    (invalid-call! validated-pre-post-defn 1)
    (invalid-call! validated-pre-post-defn "a"))
  (let [e (try (s/with-fn-validation (simple-validated-defn 2)) nil
               (catch js/Error e e))]
    (when e ;; validation can be disabled at compile time, and exception not thrown
      (is (>= (.indexOf (str e) +bad-input-str+) 0))))
  (is (= +simple-validated-defn-schema+ (s/fn-schema simple-validated-defn)))))

#?(:clj
(poly/defn ^String multi-arglist-validated-defn :- OddLongString
  "I am a multi-arglist schema fn"
  {:metadata :bla}
  ([arg0 :- OddLong]
     (str arg0))
  ([arg0 :- OddLong arg1 :- Long]
     (str (+ arg0 arg1)))))

#?(:clj
(deftest simple-validated-defn-test
  (is (= "Inputs: [arg0 :- OddLong]\n  Returns: OddLongString\n\n  I am a simple schema fn"
         (:doc (meta #'simple-validated-defn))))
  #?(:bb nil
     :default (is (= '([arg0]) (:arglists (meta #'simple-validated-defn)))))
  (is (= "Inputs: ([arg0 :- OddLong] [arg0 :- OddLong arg1 :- Long])\n  Returns: OddLongString\n\n  I am a multi-arglist schema fn"
         (:doc (meta #'multi-arglist-validated-defn))))
  #?(:bb nil
     :default (is (= '([arg0] [arg0 arg1]) (:arglists (meta #'multi-arglist-validated-defn)))))
  (s/with-fn-validation
    (testing "pre/post"
      (is (= 7 (validated-pre-post-defn 7)))
      (is (thrown-with-msg? AssertionError #"Assert failed: \(odd\? arg0\)"
                            (validated-pre-post-defn 0)))
      (is (thrown-with-msg? AssertionError #"Assert failed: \(> 10 arg0\)"
                            (validated-pre-post-defn 11)))
      (is (thrown-with-msg? AssertionError #"Assert failed: \(< 5 %\)"
                            (validated-pre-post-defn 1)))
      (invalid-call! validated-pre-post-defn "a")))
  (let [{:keys [tag schema metadata]} (meta #'simple-validated-defn)]
    (is (= tag s/Str))
    (is (= +simple-validated-defn-schema+ schema))
    (is (= metadata :bla)))
  (is (= +simple-validated-defn-schema+ (s/fn-schema simple-validated-defn)))

  (s/with-fn-validation
    (is (= "3" (simple-validated-defn 3)))
    (invalid-call! simple-validated-defn 4)
    (invalid-call! simple-validated-defn "a"))

  (is (= "4" (simple-validated-defn 4)))
  (let [^Exception e (try (s/with-fn-validation (simple-validated-defn 2)) nil (catch Exception e e))]
    (is (.contains (.getMessage e) +bad-input-str+))
    #?(:bb nil
       :default (do (is (.contains (.getClassName ^StackTraceElement (first (.getStackTrace e))) "simple_validated_defn"))
                    (is (.startsWith (.getFileName ^StackTraceElement (first (.getStackTrace e))) "poly_test.clj")))))))

(poly/defn ^:always-validate always-validated-defn :- (s/pred even?)
  [x :- (s/pred pos?)]
  (inc x))

(deftest always-validated-defn-test
  (is (= 2 (always-validated-defn 1)))
  (invalid-call! always-validated-defn 2)
  (invalid-call! always-validated-defn -1))

(poly/defn fib :- s/Int [n :- s/Int]
  (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(poly/defn fact :- s/Int [n :- s/Int ret :- s/Int]
  (if (<= n 1) ret (recur (dec n) (* ret n))))


(deftest defn-recursion-test
  (testing "non-tail recursion"
    (is (= 8 (fib 6)))
    (s/with-fn-validation
      (is (= 8 (fib 6)))))
  (testing "tail recursion"
    (is (= 120 (fact 5 1)))
    (s/with-fn-validation
      (is (= 120 (fact 5 1))))))

;; letfn

(deftest minimal-letfn-test
  (is (= "1"
         (poly/letfn
             []
           "1"))))

(deftest simple-letfn-test
  (is (= "1"
         (s/with-fn-validation
           (poly/letfn
               [(x :- s/Num [] 1)
                (y :- s/Str [m :- s/Num] (str m))]
             (y (x)))))))

(deftest unannotated-letfn-test
  (is (= "1"
         (s/with-fn-validation
           (poly/letfn
               [(x [] 1)
                (y [m] (str m))]
             (y (x)))))))

(deftest no-validation-letfn-test
  (is (= "1"
         (poly/letfn
             [(x :- s/Num [] 1)
              (y :- s/Str [m :- s/Num] (str m))]
           (y (x))))))

(deftest mutual-letfn-test
  (is (= [true false]
         (poly/letfn
           [(even [x] (if (zero? x) true (odd (dec x))))
            (odd [x] (if (zero? x) false (even (dec x))))]
           [(even 10)
            (odd 10)])))
  (is (= [true false]
         (poly/letfn
           [(even :- s/Int [x :- s/Int] (if (zero? x) true (odd (dec x))))
            (odd :- s/Int [x :- s/Int] (if (zero? x) false (even (dec x))))]
           [(even 10)
            (odd 10)])))
  (is (every? s/fn-schema
              (poly/letfn
                [(even :- s/Int [x :- s/Int] (if (zero? x) true (odd (dec x))))
                 (odd :- s/Int [x :- s/Int] (if (zero? x) false (even (dec x))))]
                [even odd]))))

(deftest error-letfn-test
  (s/with-fn-validation
    (poly/letfn
        [(x :- s/Num [] "1")
         (y :- s/Str [m :- s/Num] (str m))]
      (invalid-call! y (x))))
  (testing "without-fn-validation"
    (s/without-fn-validation
      (poly/letfn
        [(f :- s/Num [] "1")]
        (is (= "1" (f))))
      (testing "^:always-validate"
        (poly/letfn
          [(^:always-validate f :- s/Num [] "1")]
          (invalid-call! f)))))
  (testing "with-fn-validation"
    (testing "^:always-validate"
      (poly/letfn
        [(^:always-validate f :- s/Num [] "1")]
        (invalid-call! f)))
    (testing "^:never-validate"
      (s/with-fn-validation
        (poly/letfn
          [(^:never-validate f :- s/Num [] "1")]
          (is (= "1" (f)))))))
  (testing "self recursion"
    (s/with-fn-validation
      (poly/letfn
        [(f :- s/Num [] "1")]
        (invalid-call! f))
      (poly/letfn
        [(f [x :- s/Num])]
        (invalid-call! f "1"))
      (poly/letfn
        [(f
           ([] (f "1"))
           ([x :- s/Int]))]
        (invalid-call! f))))
  (testing "mutual recursion"
    (s/with-fn-validation
      (poly/letfn
        [(f [] (g "1"))
         (g [x :- s/Num])]
        (invalid-call! f))
      (poly/letfn
        [(f [] (g "1"))
         (g :- s/Num [x] x)]
        (invalid-call! f)))))

;; Primitive validation testing for JVM
#?(:clj
(do

  (def +primitive-validated-defn-schema+
    (poly/=> long OddLong))

  (poly/defn primitive-validated-defn :- long
    [^long arg0 :- OddLong]
    (inc arg0))


  (deftest simple-primitive-validated-defn-test
    (is (= +primitive-validated-defn-schema+ (s/fn-schema primitive-validated-defn)))

    #?(:bb nil
       :default (is ((ancestors (class primitive-validated-defn)) clojure.lang.IFn$LL)))
    (s/with-fn-validation
      (is (= 4 (primitive-validated-defn 3)))
       #?(:bb nil
          :default (is (= 4 (.invokePrim ^clojure.lang.IFn$LL primitive-validated-defn 3))))
      (is (thrown? Exception (primitive-validated-defn 4))))

    (is (= 5 (primitive-validated-defn 4))))

  (poly/defn another-primitive-fn :- double
    [^long arg0]
    1.0)

  (deftest another-primitive-fn-test
    #?(:bb nil
       :default (is ((ancestors (class another-primitive-fn)) clojure.lang.IFn$LD)))
    (is (= 1.0 (another-primitive-fn 10))))))


(deftest with-fn-validation-error-test
  (is (thrown? #?(:clj RuntimeException :cljs js/Error)
               (s/with-fn-validation (throw #?(:clj (RuntimeException.) :cljs (js/Error. "error"))))))
  (is (false? (s/fn-validation?))))


;; def

(deftest def-test ;; heh
  (s/def v 1)
  (is (= 1 v))
  (s/def v "doc" 2)
  (is (= 2 v))
  #?(:clj (is (= "doc" (:doc (meta #'v)))))
  (s/def v :- s/Int "doc" 3)
  (is (= 3 v))
  #?(:clj (is (= "doc" (:doc (meta #'v)))))
  (s/def v :- s/Int 3)
  #?(:clj (is (= String (:tag (meta (s/def v :- String "a"))))))
  #?(:clj (is (thrown? Exception (s/def v :- s/Int "doc" 1.0))))
  #?(:clj (is (thrown? Exception (s/def v :- s/Int 1.0))))
  #?(:clj (is (thrown? Exception (s/def ^s/Int v 1.0)))))


;; defmethod

(defmulti m #(:k (first %&)))

(deftest defmethod-unannotated-test
  (poly/defmethod m :v [m x y] (+ x y))
  (is (= 3 (m {:k :v} 1 2))))

(deftest defmethod-input-annotated
  (poly/defmethod m :v [m :- {:k s/Keyword} x :- s/Num y :- s/Num] (+ x y))
  (is (= 3
         (s/with-fn-validation (m {:k :v} 1 2)))))

(deftest defmethod-output-annotated
  (poly/defmethod m :v :- s/Num [m x y] (+ x y))
  (is (= 3
         (s/with-fn-validation (m {:k :v} 1 2)))))

(deftest defmethod-all-annotated
  (poly/defmethod m :v :- s/Num [m :- {:k s/Keyword} x :- s/Num y :- s/Num] (+ x y))
  (is (= 3
         (s/with-fn-validation (m {:k :v} 1 2)))))

(deftest defmethod-input-error-test
  (poly/defmethod m :v :- s/Num [m :- {:k s/Keyword} x :- s/Num y :- s/Num] (+ x y))
  (s/with-fn-validation (invalid-call! m {:k :v} 1 "2")))

(deftest defmethod-output-error-test
  (poly/defmethod m :v :- s/Num [m :- {:k s/Keyword} x :- s/Num y :- s/Num] "wrong")
  (s/with-fn-validation (invalid-call! m {:k :v} 1 2)))

(deftest defmethod-metadata-test
  (poly/defmethod ^:always-validate m :v :- s/Num [m :- {:k s/Keyword} x :- s/Num y :- s/Num] "wrong")
  (is (thrown? #?(:clj RuntimeException :cljs js/Error)
               (m {:k :v} 1 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Composite Schemas (test a few combinations of above)


(deftest nice-error-test
  (let [schema {:a #{[s/Int]}
                :b [(s/one s/Keyword :k) s/Int]
                :c s/Any}]
    (valid! schema {:a #{[1 2 3 4] [] [1 2]}
                    :b [:k 1 2 3]
                    :c :whatever})
    (invalid! schema {:a #{[1 2 3 4] [] [1 2] [:a :b]}
                      :b [:k]
                      :c nil}
              "{:a #{[(not (integer? :a)) (not (integer? :b))]}}")
    (invalid! schema {:a #{}
                      :b [1 :a]
                      :c nil}
              "{:b [(named (not (keyword? 1)) :k) (not (integer? :a))]}")
    (invalid! schema {:a #{}
                      :b [:k]}
              "{:c missing-required-key}")))

(s/defrecord Explainer
    [^s/Int foo ^s/Keyword bar]
  {(s/optional-key :baz) s/Keyword})

#?(:clj ;; clojurescript.test hangs on this test in phantom.js, so marking clj-only
(deftest fancy-explain-test
  (is (= (s/explain {(s/required-key 'x) s/Int
                     s/Keyword [(s/one s/Int "foo") (s/maybe Explainer)]})
         `{~'(required-key x) ~'Int
           ~'Keyword [(~'one ~'Int "foo")
                      (~'maybe
                       (~'record
                        #?(:clj Explainer
                           :cljs com.ambrosebs.schema-incubator.poly-test/Explainer)
                        {:foo ~'Int
                         :bar ~'Keyword
                         (~'optional-key :baz) ~'Keyword}))]}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Regression tests

;; map implementation takes precedence in cljs
#?(:clj
   (deftest pprint-test
     (is (= "(maybe Int)" (str/trim (with-out-str (pprint/pprint (s/maybe s/Int))))))))

(deftest print-test
  (is (= "(maybe Int)" (pr-str (s/maybe s/Int)))))

(defrecord ItemTest [first second])

(macros/defrecord-schema CacheTest [schema]
  s/Schema
  (spec [this]
    (collection/collection-spec
     (let [p (spec/precondition this #(instance? ItemTest %) #(list 'instance? ItemTest %))]
       (if-let [evf (:extra-validator-fn this)]
         (some-fn p (spec/precondition this evf #(list 'passes-extra-validation? %)))
         p))
     (fn [x] x)
     [{:schema     s/Int
       :parser     (fn [item-col m]
                     (item-col (:first m))
                     m)
       :error-wrap (fn [err] [:first (utils/error-val err)])}
      {:schema      schema
       :parser      (fn [item-col m]
                      (item-col (:second m))
                      m)
       :error-wrap  (fn [err] [:second (utils/error-val err)])}
      {:schema s/Any
       :parser (fn [_ _] nil)}]
     (fn [_ elts _] (map utils/error-val elts))))
  (explain [_]
    (list 'cache-test)))

(deftest issue-310-error-wrap-cache
  (are [schema value expected]
    (= expected (pr-str (s/check schema value)))
    (->CacheTest s/Int) (->ItemTest :a nil)
    "([:first (not (integer? :a))] [:second (not (integer? nil))])"

    (->CacheTest [s/Int]) (->ItemTest :a nil)
    "([:first (not (integer? :a))] nil)"

    (->CacheTest [s/Int]) (->ItemTest :a [nil])
    "([:first (not (integer? :a))] [:second [(not (integer? nil))]])"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Helpers for defining schemas (used in in-progress work, explanation coming soon)

(s/defschema TestFoo {:bar s/Str})

(deftest test-defschema
  (is (= 'TestFoo (:name (meta TestFoo))))
  (is (= 'com.ambrosebs.schema-incubator.poly-test (:ns (meta TestFoo)))))

(deftest schema-with-name-test
  (let [schema (s/schema-with-name {:baz s/Num} 'Baz)]
    (valid! schema {:baz 123})
    (invalid! schema {:baz "abc"})
    (is (= 'Baz (s/schema-name schema)))
    (is (= nil (s/schema-ns schema))))
  #?(:clj (is-assert! (s/schema-with-name s/Str 'Baz)
                      #"Named schema \(such as the right-most `s/defalias` arg\) must support metadata: class java.lang.Class")
     :cljs (is-assert! (s/schema-with-name nil 'Baz)
                       #"Named schema \(such as the right-most `s/defalias` arg\) must support metadata: object")))

(deftest schema-name-test
  (is (= 'TestFoo (s/schema-name TestFoo))))

(deftest schema-ns-test
  (is (= 'com.ambrosebs.schema-incubator.poly-test (s/schema-ns TestFoo))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Testing the ability to redefine schema.macros/fn-validator

(deftest soft-validation-test
  (let [the-log (atom [])
        log #(swap! the-log conj %&)]
    (with-redefs [s/fn-validator
                  (fn [dir fn-name schema checker value]
                    (when-let [err (checker value)]
                      (log dir fn-name value)))]
      (s/with-fn-validation
        (simple-validated-defn 12)
        (simple-validated-defn 13)))
    (is (= [[:input 'simple-validated-defn [12]]
            [:output 'simple-validated-defn "12"]]
           @the-log))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Testing that error messages on multimethods include the name

(defmulti ffbe878f identity)
(poly/defmethod ^:always-validate ffbe878f 42
  [x :- s/Str]
  :unreachable)

(poly/defmethod ^:always-validate other-namespace/ef408750 42
  [x :- s/Str]
  :also-unreachable)

(deftest multimethod-error-messages
  (testing "multimethods in the same namespace"
    (try
      (ffbe878f 42)
      (is false "unreachable")
      (catch Exception e
        (is (re-find #"ffbe878f"
                     (#?(:cljs .-message :clj .getMessage) e))))))
  (testing "multimethods in a different namespace"
    (try
      (other-namespace/ef408750 42)
      (is false "unreachable")
      (catch Exception e
        (is (re-find #"ef408750"
                     (#?(:cljs .-message :clj .getMessage) e)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s/defprotocol

(defprotocol ProtAssumptions
  (prot-assumptions [this a] [this a b] "foo"))

(s/defprotocol PDefProtocolTest1
  "Doc"
  (defprotocoltest1-method1
    :- s/Str
    ;; IMPORTANT don't remove arities, specifically tests 2 arities
    [this a :- s/Int]
    [this a :- s/Int, b :- s/Any]
    "doc 1")
  (defprotocoltest1-method2
    :- s/Str
    ;; IMPORTANT don't add arities, specifically tests 1 arity
    [this a :- s/Int, b :- s/Any]
    "doc 2"))

(defrecord ImplementsPDefProtocolTest1 []
  PDefProtocolTest1
  (defprotocoltest1-method1
    [this a]
    :a))

#?(:bb nil ;;https://github.com/babashka/babashka/issues/1339
   :clj
   (deftest protocol-in-another-ns
     (binding [*ns* *ns*]
       (eval `(ns ~(gensym)))
       (is (= :a (eval `(defprotocoltest1-method1 (ImplementsPDefProtocolTest1.) 1)))))))

(deftest protocol-assumptions-test
  #?(:clj
     (testing "methods never have :inline meta by default"
       (is (= {}
              (-> (var prot-assumptions)
                  meta
                  (select-keys [:inline :inline-arities]))))))
  #?(:cljs
     (testing ":protocol meta on method vars is the protocol name"
       (testing "cc/defprotocol"
         (is (= `ProtAssumptions
                (-> (var prot-assumptions)
                    meta
                    :protocol))))
       (testing "s/defprotocol"
         (is (= `PDefProtocolTest1
                (-> (var defprotocoltest1-method1)
                    meta
                    :protocol))))))
  #?(:bb nil ;;https://github.com/babashka/babashka/issues/1340
     :default (testing ":doc meta on method vars"
                (testing "cc/defprotocol"
                  (is (= "foo"
                         (-> (var prot-assumptions)
                             meta
                             :doc))))
                (testing "cc/defprotocol"
                  (is (str/ends-with?
                        (-> (var defprotocoltest1-method1)
                            meta
                            :doc)
                        "doc 1"))))))

(deftype TDefProtocolTest1 []
  PDefProtocolTest1
  (defprotocoltest1-method1 [this a] (str a))
  (defprotocoltest1-method1 [this a b] b)
  (defprotocoltest1-method2 [this a b] b))

(s/defprotocol PDefProtocolTestDefault
  ;; test two arities
  (pdefprotocol-test-default1 :- s/Str
    [this a :- s/Int]
    [this a :- s/Int, b :- s/Any])
  ;; test single arity
  (pdefprotocol-test-default2 :- s/Str
    [this a :- s/Int, b :- s/Any]))

(extend-protocol PDefProtocolTestDefault
  #?(:clj Object
     ;; default stored in "_" field of protocol method
     :cljs default)
  (pdefprotocol-test-default1
    ([this a] (str a))
    ([this a b] b))
  (pdefprotocol-test-default2 [this a b] b))

(deftest sdefprotocol-test
  (do-template
    [WRAP] (testing (pr-str 'WRAP)
             (WRAP
               (is (= "1" (defprotocoltest1-method1 (->TDefProtocolTest1) 1)))
               (testing "default dispatch"
                 (is (= "1" (pdefprotocol-test-default1 :foo 1)))
                 (is (= "1" (pdefprotocol-test-default1 :foo 1 "1")))
                 (is (= "2" (pdefprotocol-test-default1 "str" 2)))
                 (is (= "2" (pdefprotocol-test-default1 "str" 2 "2")))
                 (is (= "3" (pdefprotocol-test-default1 'a 3)))
                 (is (= "3" (pdefprotocol-test-default1 'a 3 "3"))))))
    do
    s/with-fn-validation
    s/without-fn-validation)
  (testing "metadata"
    (is (= "Doc" (-> #'PDefProtocolTest1 meta :doc)))
    #?(:bb nil ;;https://github.com/babashka/babashka/issues/1340
       :clj (is (= "doc 1" (-> #'defprotocoltest1-method1 meta :doc))))
    #?(:bb nil ;;https://github.com/babashka/babashka/issues/1340
       :clj (is (= "doc 2" (-> #'defprotocoltest1-method2 meta :doc))))
    (is (= (poly/=>* s/Str [s/Any s/Int] [s/Any s/Int s/Any])
           (s/fn-schema defprotocoltest1-method1)))
    #?(:clj (is (= (poly/=>* s/Str [s/Any s/Int] [s/Any s/Int s/Any])
                   (-> #'defprotocoltest1-method1 meta :schema)))))
  #_ ;; :inline metatdata on methods we add to prevent inlining thwarts this compile-time error
  #?(:clj
     (is (thrown-with-msg?
           Exception #"No single method"
           (eval `#(defprotocoltest1-method1 (->TDefProtocolTest1))))))
  #?(:bb nil
     :default (testing "default method errors"
                (s/with-fn-validation
                  (invalid-call! pdefprotocol-test-default1 :foo nil) ;;input
                  (invalid-call! pdefprotocol-test-default1 :foo nil "a") ;;input
                  (invalid-call! pdefprotocol-test-default1 :foo 1 :a) ;;output
                  (invalid-call! pdefprotocol-test-default1 "str" :foo) ;;input
                  (invalid-call! pdefprotocol-test-default1 "str" :foo :a) ;;input
                  (invalid-call! pdefprotocol-test-default1 "str" 1 :a)))) ;;output
  (testing "inlinable positions"
    (s/with-fn-validation
      (is (= "1" (defprotocoltest1-method1 (->TDefProtocolTest1) 1)))
      (is (= "a" (defprotocoltest1-method1 (->TDefProtocolTest1) 1 "a")))
      (is (= "a" (defprotocoltest1-method2 (->TDefProtocolTest1) 1 "a"))))
    #?(:bb nil
       :default (s/with-fn-validation
                  (invalid-call! defprotocoltest1-method1 (->TDefProtocolTest1) :a)
                  (testing "input"
                    (invalid-call! defprotocoltest1-method1 (->TDefProtocolTest1) ::foo "a"))
                  (testing "output"
                    (invalid-call! defprotocoltest1-method1 (->TDefProtocolTest1) 1 ::foo))
                  (invalid-call! defprotocoltest1-method2 (->TDefProtocolTest1) 1 ::foo)))
    ;; try a bunch of contexts and nestings to make sure inlining is defeated
    #?(:bb nil
       :clj 
       (do
         (s/with-fn-validation
           (invalid-call! (eval `(defprotocoltest1-method1 (->TDefProtocolTest1) :a))))
         (s/with-fn-validation
           (invalid-call! (eval `(let [] (defprotocoltest1-method1 (->TDefProtocolTest1) :a)))))))))

#?(:clj
   (s/defprotocol ProtocolCache
     (protocol-cache [this])))

#?(:clj
   (deftest clj-protocol-cache-test
     ;; make test repeatable
     (alter-var-root #'ProtocolCache dissoc :impls)
     (let [x 1
           ;; use partial to hold onto method reference. this acts differently
           ;; with cc/defprotocol because of CLJ-1796 (cache is never invalidated
           ;; on old references).
           call (partial protocol-cache x)]
       (extend-protocol ProtocolCache
         Number
         (protocol-cache [_] :number))
       (is (= :number (protocol-cache x) (call)))
       (extend-protocol ProtocolCache
         Long
         (protocol-cache [_] :long))
       (testing "invalidates .__methodImplCache"
         (is (= :long (protocol-cache x) (call)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic schemas

(s/defschema PolySemanticsTestSuite
  {:args-shadow-schema-variables (poly/all [x] (poly/=> x x))
   :poly-identity (poly/all [T] (poly/=> T T))
   :poly-first (poly/all [T] (poly/=> T [T]))
   :poly-map-nodot (poly/all [X Y] (poly/=> [Y] (poly/=> Y X) [X]))
   :poly-map-dot (poly/all [X Y :.. Z] (poly/=> [Z] (poly/=> Z X Y :.. Y) [X] [Y] :.. Y))
   :poly-map-dot-arities (poly/all [X Y Z S :.. T]
                                   (poly/=>* [T]
                                             [(poly/=> T X)             [X]]
                                             [(poly/=> T X Y)           [X] [Y]]
                                             [(poly/=> T X Y Z)         [X] [Y] [Z]]
                                             [(poly/=> T X Y Z S :.. S) [X] [Y] [Z] [S] :.. S]))})

(poly/defn ^:always-validate poly-semantics-test-suite
  [{:keys [args-shadow-schema-variables
           poly-identity
           poly-first
           poly-map-nodot
           poly-map-dot
           poly-map-dot-arities]} :- PolySemanticsTestSuite]
  (is (= 1 (s/with-fn-validation (args-shadow-schema-variables 1))))
  (is (= 1 (s/with-fn-validation (poly-identity 1))))
  (is (= :a (s/with-fn-validation (poly-identity :a))))
  (s/with-fn-validation (invalid-call! poly-first 1))
  (is (= 1 (s/with-fn-validation (poly-first [1]))))
  (is (= 1 (s/with-fn-validation (poly-first [1]))))
  (is (= [2 3] (s/with-fn-validation (poly-map-nodot inc [1 2]))))
  (s/with-fn-validation (invalid-call! poly-map-nodot 1 2))
  (is (= [2 3] (s/with-fn-validation (poly-map-dot inc [1 2]))))
  (is (= [3 5] (s/with-fn-validation (poly-map-dot + [1 2] [2 3]))))
  (s/with-fn-validation (invalid-call! poly-map-dot 1 2))
  (s/with-fn-validation (invalid-call! poly-map-dot + 1))
  (s/with-fn-validation (invalid-call! poly-map-dot + [1] 2))
  (is (= [2 3] (s/with-fn-validation (poly-map-dot-arities inc [1 2]))))
  (is (= [3 5] (s/with-fn-validation (poly-map-dot-arities + [1 2] [2 3]))))
  (is (= [7 10] (s/with-fn-validation (poly-map-dot-arities + [1 2] [2 3] [4 5]))))
  (is (= [13 17] (s/with-fn-validation (poly-map-dot-arities + [1 2] [2 3] [4 5] [6 7]))))
  (is (= [21 26] (s/with-fn-validation (poly-map-dot-arities + [1 2] [2 3] [4 5] [6 7] [8 9]))))
  (s/with-fn-validation (invalid-call! poly-map-dot-arities 1 1))
  (s/with-fn-validation (invalid-call! poly-map-dot-arities + 1))
  (s/with-fn-validation (invalid-call! poly-map-dot-arities + [1] 2))
  (s/with-fn-validation (invalid-call! poly-map-dot-arities + [1] [2] 3))
  (s/with-fn-validation (invalid-call! poly-map-dot-arities + [1] [2] [3] 4))
  (s/with-fn-validation (invalid-call! poly-map-dot-arities + [1] [2] [3] [4] 5)))

(poly/defn :all [x]
  args-shadow-schema-variables :- x
  [x :- x]
  x)

(poly/defn :all [T]
  poly-identity :- T
  [x :- T]
  (s/validate T x))

(poly/defn :all [T]
  poly-first :- T
  [xs :- [T]]
  (first xs))

#_ ;;TODO
(poly/defn :all [T ;TODO :- [:=> :schema :schema :schema]
              ]
  ho-schema-fn :- T
  [xs :- (T s/Int s/Bool)]
  (first xs))

(poly/defn :all [X Y]
  poly-map-nodot :- [Y]
  [f :- (poly/=> Y X)
   xs :- [X]]
  (map f xs))

(poly/defn :all [X Y :.. Z]
  poly-map-dot :- [Z]
  [f :- (poly/=> Z X Y :.. Y)
   xs :- [X]
   & yss :- [Y] :.. Y]
  (apply map f xs yss))

(poly/defn :all [X Y Z S :.. T]
  poly-map-dot-arities :- [T]
  ([f :- (poly/=> T X)
    xs :- [X]]
   (map f xs))
  ([f :- (poly/=> T X Y)
    xs :- [X]
    ys :- [Y]]
   (map f xs ys))
  ([f :- (poly/=> T X Y Z)
    xs :- [X]
    ys :- [Y]
    zs :- [Z]]
   (map f xs ys zs))
  ([f :- (poly/=> T X Y Z S :.. S)
    xs :- [X]
    ys :- [Y]
    zs :- [Z]
    & ss :- [S] :.. S]
   (apply map f xs ys zs ss)))

(deftest explain-all-test
  (is (= '(all [x] (poly/=> x)) (s/explain (poly/all [x] (poly/=> x)))))
  ;;FIXME ideally (all [T] (poly/=> T))
  (is (= '(all [T] (schema.core/->FnSchema T [[(schema.core/one T (quote x))]]))
         (s/explain (s/fn-schema poly-identity)))))

(deftest all-test
  #?(:clj (is (thrown-with-msg? Exception #"Duplicate variable x in binder: \[x x\]"
                                (eval `(poly/all [~'x ~'x] (poly/=> x)))))))

(deftest instantiate-test
  (is (= (@#'poly/instantiate (poly/all [a] (poly/=> a))
                           s/Int)
         (poly/=> s/Int)))
  (is (not= (@#'poly/instantiate (poly/all [a] (poly/=> a))
                              s/Bool)
            (poly/=> s/Int)))
  (is (= (@#'poly/instantiate (poly/all [a] (poly/=> a a))
                           s/Int)
         (poly/=> s/Int s/Int)))
  (is (= (@#'poly/instantiate (poly/all [a b] (poly/=> a b a b))
                              s/Int s/Bool)
         (poly/=> s/Int s/Bool s/Int s/Bool)))
  (is (= (@#'poly/instantiate (poly/all [a :.. b] (poly/=> b a :.. a))
                              [s/Int s/Any] s/Bool)
         (poly/=> s/Bool s/Int s/Any)))
  #_ ;;TODO
  (is (= (@#'poly/instantiate (poly/all [a :.. b :.. c] (poly/=> c a :.. a b :.. b))
                              [s/Int s/Any] [s/Bool s/Any] s/Str)
         (poly/=> s/Str s/Int s/Any s/Bool s/Any s/Str)))
  (is (= '(=> Int Int)
         (s/explain (@#'poly/instantiate (s/fn-schema poly-identity) s/Int))))
  (is (thrown-with-msg? Exception #"Wrong number of arguments"
                        (@#'poly/instantiate (s/fn-schema poly-map-nodot) s/Int))))

(deftest inst-most-general-test 
  (is (= '(=> Any Any)
         (s/explain (@#'poly/inst-most-general (s/fn-schema poly-identity)))))
  (is (= '(=> Any [Any])
         (s/explain (@#'poly/inst-most-general (s/fn-schema poly-first)))))
  (is (= '(=> [Any] (=> Any Any) [Any])
         (s/explain (@#'poly/inst-most-general (s/fn-schema poly-map-nodot)))))
  (is (= '(=> [Any] (=> Any Any & [Any]) [Any] & [[Any]])
         (s/explain (@#'poly/inst-most-general (s/fn-schema poly-map-dot))))))

(deftest poly-defn-semantics-test
  (poly-semantics-test-suite
    {:args-shadow-schema-variables args-shadow-schema-variables
     :poly-identity poly-identity
     :poly-first poly-first
     :poly-map-nodot poly-map-nodot
     :poly-map-dot poly-map-dot
     :poly-map-dot-arities poly-map-dot-arities}))

(deftest poly-fn-semantics-test
  (testing "no name"
    (is (= 1 ((poly/fn :all [T] [x :- T] x) 1)))
    (poly-semantics-test-suite
      {:args-shadow-schema-variables (poly/fn :all [x] :- x [x :- x] x)
       :poly-identity (poly/fn :all [T] :- T [x :- T] (s/validate T x))
       :poly-first (poly/fn :all [T] :- T [xs :- [T]] (first xs))
       :poly-map-nodot (poly/fn :all [X Y] :- [Y] [f :- (poly/=> Y X) xs :- [X]] (map f xs))
       :poly-map-dot (poly/fn :all [X Y :.. Z] :- [Z] [f :- (poly/=> Z X Y :.. Y) xs :- [X] & yss :- [Y] :.. Y] (apply map f xs yss))
       :poly-map-dot-arities (poly/fn :all [X Y Z S :.. T]
                               :- [T]
                               ([f :- (poly/=> T X) xs :- [X]] (map f xs))
                               ([f :- (poly/=> T X Y) xs :- [X] ys :- [Y]] (map f xs ys))
                               ([f :- (poly/=> T X Y Z) xs :- [X] ys :- [Y] zs :- [Z]] (map f xs ys zs))
                               ([f :- (poly/=> T X Y Z S :.. S) xs :- [X] ys :- [Y] zs :- [Z] & ss :- [S] :.. S] (apply map f xs ys zs ss)))}))
  (testing "with name"
    (poly-semantics-test-suite
      {:args-shadow-schema-variables (poly/fn :all [x] args-shadow-schema-variables :- x [x :- x] x)
       :poly-identity (poly/fn :all [T] poly-identity :- T [x :- T] (s/validate T x))
       :poly-first (poly/fn :all [T] poly-first :- T [xs :- [T]] (first xs))
       :poly-map-nodot (poly/fn :all [X Y] poly-map-nodot :- [Y] [f :- (poly/=> Y X) xs :- [X]] (map f xs))
       :poly-map-dot (poly/fn :all [X Y :.. Z] poly-map-dot :- [Z] [f :- (poly/=> Z X Y :.. Y) xs :- [X] & yss :- [Y] :.. Y] (apply map f xs yss))
       :poly-map-dot-arities (poly/fn :all [X Y Z S :.. T]
                               poly-map-dot-arities :- [T]
                               ([f :- (poly/=> T X) xs :- [X]] (map f xs))
                               ([f :- (poly/=> T X Y) xs :- [X] ys :- [Y]] (map f xs ys))
                               ([f :- (poly/=> T X Y Z) xs :- [X] ys :- [Y] zs :- [Z]] (map f xs ys zs))
                               ([f :- (poly/=> T X Y Z S :.. S) xs :- [X] ys :- [Y] zs :- [Z] & ss :- [S] :.. S] (apply map f xs ys zs ss)))})))

(deftest poly-letfn-semantics-test
  (poly-semantics-test-suite
    {:args-shadow-schema-variables (poly/letfn [(:all [x] args-shadow-schema-variables :- x [x :- x] x)]
                                     args-shadow-schema-variables)
     :poly-identity (poly/letfn [(:all [T] poly-identity :- T [x :- T] (s/validate T x))]
                      poly-identity)
     :poly-first (poly/letfn [(:all [T] poly-first :- T [xs :- [T]] (first xs))]
                   poly-first)
     :poly-map-nodot (poly/letfn [(:all [X Y] poly-map-nodot :- [Y] [f :- (poly/=> Y X) xs :- [X]] (map f xs))]
                       poly-map-nodot)
     :poly-map-dot (poly/letfn [(:all [X Y :.. Z] poly-map-dot :- [Z] [f :- (poly/=> Z X Y :.. Y) xs :- [X] & yss :- [Y] :.. Y] (apply map f xs yss))]
                     poly-map-dot)
     :poly-map-dot-arities (poly/letfn [(:all [X Y Z S :.. T]
                                          poly-map-dot-arities :- [T]
                                          ([f :- (poly/=> T X) xs :- [X]] (map f xs))
                                          ([f :- (poly/=> T X Y) xs :- [X] ys :- [Y]] (map f xs ys))
                                          ([f :- (poly/=> T X Y Z) xs :- [X] ys :- [Y] zs :- [Z]] (map f xs ys zs))
                                          ([f :- (poly/=> T X Y Z S :.. S) xs :- [X] ys :- [Y] zs :- [Z] & ss :- [S] :.. S] (apply map f xs ys zs ss)))]
                             poly-map-dot-arities)}))


(deftest return-schema-test
  (is (= (poly/return-schema (s/=> s/Int))
         s/Int))
  (is (= (poly/return-schema (s/=> s/Bool s/Int))
         s/Bool))
  (is (= (poly/return-schema (s/=> s/Bool & [s/Int]))
         s/Bool))
  (is (= (poly/return-schema (poly/=> s/Int))
         s/Int))
  (is (= (poly/return-schema (poly/=> s/Bool s/Int))
         s/Bool))
  (is (= (poly/return-schema (poly/=> s/Bool & [s/Int]))
         s/Bool))
  (is (thrown? #?(:clj Error :cljs js/Error) (poly/return-schema s/Int))))

;; idea: (poly/tuple A :.. A B :*)

(deftest args-schema-test
  (let [s (poly/args-schema (s/=> s/Any s/Int s/Bool))]
    (is (= [1 true] (s/validate s [1 true])))
    (is (thrown? Exception (s/validate s [true 1]))))
  (let [s (poly/args-schema (poly/all [a] (s/=> s/Any a s/Bool)))]
    (is (= [1 true] (s/validate s [1 true])))
    (is (= [:a false] (s/validate s [:a false])))
    (is (thrown? Exception (s/validate s [1 :a])))))

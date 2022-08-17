(ns com.ambrosebs.schema-incubator.poly
  ;schema.core
  ;; don't exclude def because it's not a var.
  (:refer-clojure :exclude [Keyword Symbol Inst atom defprotocol defrecord defn letfn defmethod fn MapEntry ->MapEntry])
  (:require
   [clojure.core :as cc]
   #?(:clj [clojure.pprint :as pprint])
   [clojure.string :as str]
   [schema.core :as s]
   #?(:clj [schema.macros :refer [assert! compile-fn-validation? defrecord-schema if-bb if-cljs]])
   #?(:clj [com.ambrosebs.schema-incubator.poly.macros :as macros])
   [schema.utils :as utils]
   [schema.spec.core :as spec :include-macros true]
   [schema.spec.leaf :as leaf])
  #?(:cljs (:require-macros [com.ambrosebs.schema-incubator.poly.macros :as macros]
                            [schema.macros :refer [assert! compile-fn-validation? defrecord-schema if-bb if-cljs]]
                            com.ambrosebs.schema-incubator.poly)))

#?(:clj (set! *warn-on-reflection* true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Polymorphic Schemas


(clojure.core/defrecord ^:no-doc AnyDotted [schema])

(declare ^:private inst-most-general)

(defrecord-schema PolySchema [decl parsed-decl schema-form inst->schema]
  s/Schema
  (spec [this] (s/spec (inst-most-general this)))
  (explain [this] (list 'all decl schema-form)))

(cc/defn instantiate
  "Instantiate a polymorphic schema with schemas."
  [{:keys [inst->schema parsed-decl] :as for-all-schema} & schemas]
  {:pre [(instance? PolySchema for-all-schema)]}
  (assert! (= (count schemas) (count parsed-decl))
           "Wrong number of arguments to instantiate schema %s: expected %s, actual %s"
           (s/explain for-all-schema)
           (count parsed-decl)
           (count schemas))
  (apply inst->schema schemas))

(defn- most-general-insts [=>-schema]
  {:pre [(instance? PolySchema =>-schema)]}
  (mapv (clojure.core/fn [[sym {:keys [kind]}]]
          (case kind
            :schema s/Any
            :.. (->AnyDotted s/Any)
            (throw (ex-info (str "Unknown kind: " kind)
                            {}))))
        (:parsed-decl =>-schema)))

(defn- inst-most-general [=>-schema]
  {:pre [(instance? PolySchema =>-schema)]}
  (apply instantiate =>-schema (most-general-insts =>-schema)))
 
(clojure.core/defn poly-schema? [v]
  (instance? PolySchema v))

(defmacro all
  "Create a polymorphic function schema.
  
   Binder declaration is a vector of polymorphic variables and its kinds.

   Schema variables have a 'kind' that classify what it represents.
   :- assigns a kind to a polymorphic variable. By default, polymorphic variables are kind :schema.

   1. [T :- :schema]   represents a Schema, eg., s/Any, s/Int, (s/=> s/Int s/Bool)
   2. [T :- :..]       represents a vector of schemas, often to represent heterogenous rest arguments
                       eg., [s/Int s/Bool]

   [T :..] is sugar for [T :- :..]"
  [decl schema]
  {:pre [(vector? decl)]}
  (let [parsed-decl (macros/parse-poly-binder decl)]
    `(->PolySchema
       '~decl
       '~parsed-decl
       '~schema
       (clojure.core/fn ~(mapv first parsed-decl) ~schema))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function Schemas

;; A function schema describes a function of one or more arities.
;; The function can only have a single output schema (across all arities), and each input
;; schema is a sequence schema describing the argument vector.

#?(:clj
(defmacro =>*
  "Produce a function schema from an output schema and a list of arity input schema specs,
   each of which is a vector of argument schemas, ending with an optional '& more-schema'
   specification where more-schema must be a sequence schema.

   Dotted schemas are allowed as the final arguments, and will be expanded into either fixed
   or rest arguments upon evaluation.

   For example, if `Y :..` is in scope then (=> Z X [Y] :.. Y) represents the following functions:
    (=> Z X)
    (=> Z X [Y0])
    (=> Z X [Y0] [Y1])
    (=> Z X [Y0] [Y1] [Y1])
    ...etc

   Depending on the instantiation of Y, the schema at runtime will be one of the above with
   concretized Y's or the following schema that encapsulates them all:
    (=> Z X & [[s/Any]])

   Currently function schemas are purely descriptive; there is no validation except for
   functions defined directly by s/fn or s/defn"
  [output-schema & arity-schema-specs]
  `(s/make-fn-schema ~output-schema ~(mapv macros/parse-arity-spec arity-schema-specs))))

#?(:clj
(defmacro =>
  "Convenience macro for defining function schemas with a single arity; like =>*, but
   there is no vector around the argument schemas for this arity."
  [output-schema & arg-schemas]
  `(=>* ~output-schema ~(vec arg-schemas))))

(defn- split-arities
  "Internal"
  [=>-schema]
  {:pre [(instance? schema.core.FnSchema =>-schema)]}
  (let [;; sorted by arity size
        input-schemas (vec (:input-schemas =>-schema))
        _ (assert (seq input-schemas) (pr-str =>-schema))
        has-varargs? (not (instance? schema.core.One (-> input-schemas peek peek)))]
    {:fixed-input-schemas (cond-> input-schemas
                            has-varargs? pop)
     :variable-input-schema (when has-varargs?
                              (peek input-schemas))}))


;; TODO convert (s/one (s/named .. b) a) to (s/one .. a) to work around
;; s/=> gensymming.
;;  
;;  ((gen/generate
;;     (generator (s/=> s/Int (s/named s/Int 'something))))
;;   :foo)
;;  ;=> Value does not match schema: [(named (named (not (integer? :foo)) something) arg0)]
(cc/defn args-schema
  "Returns the schema of the arguments to the => schema, or =>/all
  instantiated with s/Any."
  [=>-schema]
  {:post [(satisfies? s/Schema %)]}
  (let [=>-schema (cond-> =>-schema
                    (instance? PolySchema =>-schema) inst-most-general)
        _ (assert (instance? schema.core.FnSchema =>-schema) (pr-str =>-schema))
        {:keys [fixed-input-schemas variable-input-schema]} 
        (split-arities =>-schema)]
    (apply s/conditional
           (concat
             (mapcat (cc/fn [fixed-input-schema]
                       (let [arity (count fixed-input-schema)]
                         [(every-pred vector? #(= arity (count %)))
                          fixed-input-schema]))
                     fixed-input-schemas)
             (some->> variable-input-schema
                      (vector :else))))))

(cc/defn return-schema
  "Returns the schema of the return value of the => schema,
  or =>/all instantiated with s/Any."
  [=>-schema]
  {:post [(satisfies? s/Schema %)]}
  (let [=>-schema (cond-> =>-schema
                    (instance? PolySchema =>-schema) inst-most-general)
        _ (assert (instance? schema.core.FnSchema =>-schema))]
    (:output-schema =>-schema)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schematized defrecord and (de,let)fn macros

#?(:clj
(defmacro defrecord
  "Define a record with a schema.

   In addition to the ordinary behavior of defrecord, this macro produces a schema
   for the Record, which will automatically be used when validating instances of
   the Record class:

   (m/defrecord FooBar
    [foo :- Int
     bar :- String])

   (schema.utils/class-schema FooBar)
   ==> (record user.FooBar {:foo Int, :bar java.lang.String})

   (s/check FooBar (FooBar. 1.2 :not-a-string))
   ==> {:foo (not (integer? 1.2)), :bar (not (instance? java.lang.String :not-a-string))}

   See (doc schema.core) for details of the :- syntax for record elements.

   Moreover, optional arguments extra-key-schema? and extra-validator-fn? can be
   passed to augment the record schema.
    - extra-key-schema is a map schema that defines validation for additional
      key-value pairs not in the record base (the default is to not allow extra
       mappings).
    - extra-validator-fn? is an additional predicate that will be used as part
      of validating the record value.

   The remaining opts+specs (i.e., protocol and interface implementations) are
   passed through directly to defrecord.

   Finally, this macro replaces Clojure's map->name constructor with one that is
   more than an order of magnitude faster (as of Clojure 1.5), and provides a
   new strict-map->name constructor that throws or drops extra keys not in the
   record base."
  {:arglists '([name field-schema extra-key-schema? extra-validator-fn? & opts+specs])}
  [name field-schema & more-args]
  (apply macros/emit-defrecord 'clojure.core/defrecord &env name field-schema more-args)))

#?(:clj
(defmacro fn
  "s/fn : s/defn :: clojure.core/fn : clojure.core/defn

   See (doc s/defn) for details.

   Additional gotchas and limitations:
    - Like s/defn, the output schema must go on the fn name. If you
      don't supply a name, schema will gensym one for you and attach
      the schema.
    - Unlike s/defn, the function schema is stored in metadata on the
      fn. The implications of this differ per platform:
      :clj   The resulting function has the same performance characteristics
             as clojure.core/fn. Additionally, the following invariant
             holds for all parameters and schema annotations:
               (let [f (s/fn this ... [...] this)]
                 (assert (identical? f (f ...))))
      :cljs  Returns a wrapper function that forwards arguments positionally
             up to 20 arguments, and then via `apply` beyond 20 arguments.
             See `cljs.core/with-meta` and `cljs.core.MetaFn`."
  [& fn-args]
  (let [[leading-opts fn-args] (macros/extract-leading-fn-kv-pairs fn-args)
        fn-args (if (symbol? (first fn-args))
                  fn-args
                  (cons (gensym "fn") fn-args))
        [name more-fn-args] (macros/extract-arrow-schematized-element &env fn-args)
        name (vary-meta name merge leading-opts)
        {:keys [outer-bindings schema-form fn-body]} (macros/process-fn- &env name more-fn-args)]
    `(let [~@outer-bindings
           ;; let bind to work around https://clojure.atlassian.net/browse/CLJS-968
           f# ~(vary-meta `(clojure.core/fn ~name ~@fn-body)
                          #(assoc (merge (meta &form) %)
                                  :schema schema-form))]
       f#))))

#?(:clj
(defmacro defn
  "Like clojure.core/defn, except that schema-style typehints can be given on
   the argument symbols and on the function name (for the return value).

   You can call s/fn-schema on the defined function to get its schema back, or
   use with-fn-validation to enable runtime checking of function inputs and
   outputs.

   (s/defn foo :- s/Num
    [x :- s/Int
     y :- s/Num]
    (* x y))

   (s/fn-schema foo)
   ==> (=> java.lang.Number Int java.lang.Number)

   (s/with-fn-validation (foo 1 2))
   ==> 2

   (s/with-fn-validation (foo 1.5 2))
   ==> Input to foo does not match schema: [(named (not (integer? 1.5)) x) nil]

   See (doc schema.core) for details of the :- syntax for arguments and return
   schemas.

   You can use :all to make a polymorphic schema by binding polymorphic variables.
   See `s/all` for more about polymorphic variables.
 
   The polymorphic variables are scoped inside the function body. Note, they will usually be bound
   to their most general values (eg., s/Any) at runtime. This strategy also informs how
   `s/with-fn-validation` treats polymorphic variables. However, the values of schema
   variables should always be treated as opaque.

   In the body of the function, names provided by argument vectors may shadow polymorphic variables.

   (s/defn :all [T]
    my-identity :- T
    [x :- T]
    ;; from here, (destructured) arguments shadow polymorphic variables
    ...
    ;; usually equivalent to (s/validate s/Any x)
    (s/validate T x))

   (s/fn-schema my-identity)
   ==> (all [T] (=> T T))


   Rest parameter schemas may be expanded via dotted variables by placing :.. after
   the schema following the '&'. The following example demonstrates most usages of
   dotted variables:

   (s/defn :all [X Y :.. Z]
     map :- [Z]
     [f :- (=> Z X Y :.. Y)
      xs :- [X]
      & xss :- [Y] :.. Y]
     (apply map f xs xss))

   The schema for the above function encapsulates the following schemas:
    (all [X          Z] (=> [Z] (=> Z          X) [X]))
    (all [X Y0       Z] (=> [Z] (=> Z Y0       X) [X] [Y0]))
    (all [X Y0 Y1    Z] (=> [Z] (=> Z Y0 Y1    X) [X] [Y0] [Y1]))
    (all [X Y0 Y1 Y2 Z] (=> [Z] (=> Z Y0 Y1 Y2 X) [X] [Y0] [Y1] [Y0]))
    ...etc


   The overhead for checking if run-time validation should be used is very
   small -- about 5% of a very small fn call.  On top of that, actual
   validation costs what it costs.

   You can also turn on validation unconditionally for this fn only by
   putting ^:always-validate metadata on the fn name.

   Gotchas and limitations:
    - The output schema always goes on the fn name, not the arg vector. This
      means that all arities must share the same output schema. Schema will
      automatically propagate primitive hints to the arg vector and class hints
      to the fn name, so that you get the behavior you expect from Clojure.
    - All primitive schemas will be passed through as type hints to Clojure,
      despite their legality in a particular position.  E.g.,
        (s/defn foo [x :- int])
      will fail because Clojure does not allow primitive ints as fn arguments;
      in such cases, use the boxed Classes instead (e.g., Integer).
    - Schema metadata is only processed on top-level arguments.  I.e., you can
      use destructuring, but you must put schema metadata on the top-level
      arguments, not the destructured variables.

      Bad:  (s/defn foo [{:keys [x :- s/Int]}])
      Good: (s/defn foo [{:keys [x]} :- {:x s/Int}])
    - Only a specific subset of rest-arg destructuring is supported:
      - & rest works as expected
      - & [a b] works, with schemas for individual elements parsed out of the binding,
        or an overall schema on the vector
      - & {} is not supported.
    - Unlike clojure.core/defn, a final attr-map on multi-arity functions
      is not supported."
  [& defn-args]
  (let [[name & more-defn-args] (macros/normalized-defn-args &env defn-args)
        {:keys [doc tag] :as standard-meta} (meta name)
        {:keys [outer-bindings schema-form fn-body arglists raw-arglists]} (macros/process-fn- &env name more-defn-args)]
    `(let ~outer-bindings
       (let [ret# (clojure.core/defn ~(with-meta name {})
                    ~(assoc (apply dissoc standard-meta ::macros/poly-binder (when (macros/primitive-sym? tag) [:tag]))
                       :doc (str
                             (str "Inputs: " (if (= 1 (count raw-arglists))
                                               (first raw-arglists)
                                               (apply list raw-arglists)))
                             (when-let [ret (when (= (second defn-args) :-) (nth defn-args 2))]
                               (str "\n  Returns: " ret))
                             (when doc (str  "\n\n  " doc)))
                       :raw-arglists (list 'quote raw-arglists)
                       :arglists (list 'quote arglists)
                       :schema schema-form)
                    ~@fn-body)]
         (utils/declare-class-schema! (utils/fn-schema-bearer ~name) ~schema-form)
         ret#)))))

#?(:clj
(defmacro defmethod
  "Like clojure.core/defmethod, except that schema-style typehints can be given on
   the argument symbols and after the dispatch-val (for the return value).

   See (doc s/defn) for details.

   Examples:

     (s/defmethod mymultifun :a-dispatch-value :- s/Num [x :- s/Int y :- s/Num] (* x y))

     ;; You can also use meta tags like ^:always-validate by placing them
     ;; before the multifunction name:

     (s/defmethod ^:always-validate mymultifun :a-dispatch-value [x y] (* x y))"
  [multifn dispatch-val & fn-tail]
  (let [methodfn `(fn ~(with-meta (gensym (str (name multifn) "__")) (meta multifn)) ~@fn-tail)]
    `(if-cljs
       (cljs.core/-add-method
         ~(with-meta multifn {:tag 'cljs.core/MultiFn})
         ~dispatch-val
         ~methodfn)
       ~#?(:bb `(let [methodfn# ~methodfn]
                  (clojure.core/defmethod ~multifn ~dispatch-val [& args#] (apply methodfn# args#)))
           :default `(. ~(with-meta multifn {:tag 'clojure.lang.MultiFn})
                        addMethod
                        ~dispatch-val
                        ~methodfn))))))

#?(:clj
(defmacro defprotocol
  "Like clojure.core/defprotocol, except schema-style typehints can be provided for
  the argument symbols and after method names (for output schemas).

  ^:always-validate and ^:never-validate metadata can be specified for all
  methods on the protocol name. If specified on the method name, ignores
  the protocol name metatdata and uses the method name metadata.

  Examples:

    (s/defprotocol MyProtocol
      \"Docstring\"
      :extend-via-metadata true
      (^:always-validate method1 :- s/Int
        [this a :- s/Bool]
        [this a :- s/Any, b :- s/Str]
        \"Method doc2\")
      (^:never-validate method2 :- s/Int
        [this]
        \"Method doc2\"))
  
  Gotchas and limitations:
  - Implementation details are used to instrument protocol methods for schema
    checking. This is tested against a variety of platforms and versions,
    however if this is problematic for your environment, use
    *elide-defprotocol-instrumentation* to disable such instrumentation
    (either at compile-time or runtime depending on your needs).
    In ClojureScript, method var metadata will be overwritten unless disabled
    at compile-time. 
  - :schema metadata on protocol method vars is only supported in Clojure.
  - Clojure will never inline protocol methods, as :inline metadata is added to protocol
    methods designed to defeat potential short-circuiting of schema checks. This also means
    compile-time errors for arity errors are suppressed (eg., `No single method` errors).
  - Methods cannot be instrumented in babashka due to technical limitations."
  [& name+opts+sigs]
  (let [{:keys [pname doc opts parsed-sigs]} (macros/process-defprotocol &env name+opts+sigs)
        sigs (map :sig parsed-sigs)
        defprotocol-form `(clojure.core/defprotocol
                            ~pname
                            ~@(when doc [doc])
                            ~@opts
                            ~@sigs)
        instrument? (s/instrument-defprotocol?)]
    `(do ~defprotocol-form
         ;; put everything that relies on protocol implementation details here so the user can
         ;; turn it off for whatever reason.
         ~@(when instrument?
             ;; in bb, protocol methods are multimethods. there's no way to be notified when
             ;; a multimethod is extended so we're stuck.
             #?(:bb nil
                :default (map (fn [{:keys [method-name instrument-method]}]
                                `(when (s/instrument-defprotocol?)
                                   ~instrument-method))
                              parsed-sigs)))
         ;; we always want s/fn-schema to work on protocol methods and have :schema
         ;; metadata on the var in Clojure.
         ~@(map (fn [{:keys [method-name schema-form]}]
                  `(let [fn-schema# ~schema-form]
                     ;; utils/declare-class-schema! works for subtly different reasons for each platform:
                     ;; :clj -- while CLJ-1796 means a method will change its identity after -reset-methods,
                     ;;         it does not change its class, as the same method builder is used each time.
                     ;;         fn-schema-bearer uses the class in :clj, so we're ok.
                     ;; :cljs -- method identity never changes, and fn-schema-bearer uses function identity in :cljs.
                     ;; :bb -- methods are multimethods which have defonce semantics are always class MultiFn. Object identity is used.
                     (utils/declare-class-schema! (if-bb ~method-name (utils/fn-schema-bearer ~method-name)) fn-schema#)
                     ;; also add :schema metadata like s/defn
                     (if-cljs
                       nil
                       (alter-meta! (var ~method-name) assoc :schema fn-schema#))))
                parsed-sigs)
         ~pname))))

#?(:clj
(defmacro letfn
  "s/letfn : s/fn :: clojure.core/letfn : clojure.core/fn
  
  Gotchas:
  - s/fn-schema will only work on direct references to the bindings
    inside the body. It will not work on intermediate calls between bindings."
  [fnspecs & body]
  (let [{:keys [outer-bindings
                fnspecs
                inner-bindings]}
        (reduce (clojure.core/fn [acc fnspec]
                  (let [[leading-opts fnspec] (macros/extract-leading-fn-kv-pairs fnspec)
                        [name more-fn-args] (macros/extract-arrow-schematized-element &env fnspec)
                        name (vary-meta name merge leading-opts)
                        {:keys [outer-bindings schema-form fn-body]} (macros/process-fn- &env name more-fn-args)]
                    (-> acc
                        (update :outer-bindings into outer-bindings)
                        (update :fnspecs conj (cons name fn-body))
                        (update :inner-bindings conj name `(s/schematize-fn
                                                             ~name
                                                             ~schema-form)))))
                {:outer-bindings []
                 :fnspecs []
                 :inner-bindings []}
                fnspecs)]
    `(let ~outer-bindings
       (clojure.core/letfn
         ~fnspecs
         (let ~inner-bindings
           (do ~@body)))))))

#?(:clj
(set! *warn-on-reflection* false))

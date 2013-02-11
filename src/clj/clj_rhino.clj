(ns clj-rhino
  (:refer-clojure :exclude (eval get get-in set!))
  (:import [org.mozilla.javascript Context UniqueTag NativeArray NativeObject
            BaseFunction]
           [org.marianoguerra.rhino TimedContextFactory]))

(defprotocol RhinoConvertible
  (-to-rhino [object scope ctx] "convert a value to a rhino compatible type"))

(defprotocol ClojureConvertible
  (-from-rhino [object]
           "convert a value from rhino to a more clojure friendly representation"))

(defn with-context [fun]
  "create a context call fun with it and safelly exit the context"
  (let [ctx (Context/enter)]
    (try
      (fun ctx)
      (finally (Context/exit)))))

(defn with-context-if-nil [ctx fun]
     "create a context if ctx is nil, otherwise use ctx and call fun with it,
     exit safelly after if ctx was created here, otherwise is up to the caller
     (which should be inside a with-context somewhere up the call stack)"
  (if ctx
    (fun ctx)
    (with-context fun)))

(defn to-js [obj scope & [ctx]]
  "convert obj to a rhino compatible object"
  (with-context-if-nil ctx #(-to-rhino obj scope %)))

(defn from-js [obj]
  "convert obj from rhino into a clojure friendly representation"
  (-from-rhino obj))

(defn- return-self [obj scope ctx] obj)
  
(defn to-js-array [arr scope ctx]
  (.newArray ctx scope (to-array (map #(to-js % scope ctx) arr))))

(defn to-js-ratio [obj scope ctx] (double obj))
(defn to-js-object [obj scope ctx]
  (let [js-obj (.newObject ctx scope)]
    (dorun (map (fn [[key val]]
                  (let [key (name key)
                        val (to-js val scope ctx)]

                    (.put js-obj key js-obj val)))
                obj))
    js-obj))

(defn to-js-generic [obj scope ctx] 
  (if (.isArray (class obj))
    (to-js (seq obj) scope ctx)
    (throw (Exception. (str "Don't know how to convert to rhino " (class obj))))))

(defn make-fn [fun]
  "return an object that can be used as a function in rhino,
  fun must receive the following arguments [ctx scope this args]
  args will be passed through from-js before calling fun and the result 
  through to-js"

  (proxy [BaseFunction] []
    (call [ctx scope this args]
      (to-js (fun ctx scope this (from-js args)) scope ctx))))

(defn wrap-plain-fn [fun]
 "wrap fun with a function that accepts all the internal arguments
 and does (apply fun args)
 with this version you can expose any plain clojure function but you don't
 have access to scope, context or this"
  (make-fn (fn [ctx scope this args]
             (apply fun args))))

(extend nil                    RhinoConvertible {:-to-rhino return-self})
(extend java.lang.Boolean      RhinoConvertible {:-to-rhino return-self})

;; Numbers
(extend java.lang.Number       RhinoConvertible {:-to-rhino return-self})
(extend clojure.lang.Ratio     RhinoConvertible {:-to-rhino to-js-ratio})
(extend java.math.BigInteger   RhinoConvertible {:-to-rhino return-self})
(extend java.math.BigDecimal   RhinoConvertible {:-to-rhino return-self})

;; Symbols, Keywords, and Strings
(extend clojure.lang.Named     RhinoConvertible {:-to-rhino (fn [obj scope ctx]
                                                              (name obj))})
(extend java.lang.CharSequence RhinoConvertible {:-to-rhino (fn [obj scope ctx]
                                                              (.toString obj))})

;; Collections
(extend java.util.Map          RhinoConvertible {:-to-rhino to-js-object})
(extend java.util.Collection   RhinoConvertible {:-to-rhino to-js-array})

;; Functions
(extend clojure.lang.Fn        RhinoConvertible {:-to-rhino (fn [obj scope ctx]
                                                              (make-fn obj))})

;; Maybe a Java array, otherwise fail
(extend java.lang.Object       RhinoConvertible {:-to-rhino to-js-generic})

(defn- entryset-to-pair [entry]
  (let [str-key (.getKey entry)
        key (keyword str-key)
        js-value (.getValue entry)
        val (from-js js-value)]

  [key val]))

(defn- from-js-object [obj]
  (apply hash-map (mapcat entryset-to-pair (.entrySet obj))))

(extend nil                    ClojureConvertible {:-from-rhino identity})
(extend java.lang.Boolean      ClojureConvertible {:-from-rhino identity})
(extend java.lang.Number       ClojureConvertible {:-from-rhino identity})
(extend java.math.BigInteger   ClojureConvertible {:-from-rhino identity})
(extend java.math.BigDecimal   ClojureConvertible {:-from-rhino identity})
(extend java.lang.CharSequence ClojureConvertible {:-from-rhino identity})
(extend java.lang.Object       ClojureConvertible {:-from-rhino identity})
; NOTE: undefined and null will return nil, there are other tags which should not 
; be produced from a js program
; https://github.com/mozilla/rhino/blob/master/src/org/mozilla/javascript/UniqueTag.java
(extend UniqueTag              ClojureConvertible {:-from-rhino (fn [obj] nil)})
(extend NativeObject           ClojureConvertible {:-from-rhino from-js-object})
(extend NativeArray            ClojureConvertible {:-from-rhino (comp vec (partial map from-js))})

(def insecure-vars ["isXMLName" "uneval" "InternalError" "JavaException"
                    "With" "Call" "Script" "Iterator" "StopIteration",
                    "Packages" "java" "javax" "org" "com" "edu" "net"
                    "getClass" "JavaAdapter" "JavaImporter" "Continuation"
                    "XML" "XMLList" "Namespace" "QName"])

(defn eval [scope code & {:keys [ctx filename line-number sec-domain]}]
  (with-context-if-nil ctx
    (fn [ctx1]
      (.evaluateString ctx1 scope code
                       (or filename "<eval>")
                       (or line-number 1) sec-domain))))

(defn eval-timeout [scope code timeout-ms & {:keys [filename line-number sec-domain]}]
  (let [filename (or filename "<eval>")
        line-number (or line-number 1)
        factory (TimedContextFactory. timeout-ms)
        ctx (.enterContext factory)]
    (try
      (.evaluateString ctx scope code filename line-number sec-domain)
      (finally (Context/exit)))))

(defn call-timeout-raw [scope fun timeout-ms & args]
  (let [factory (TimedContextFactory. timeout-ms)
        ctx (.enterContext factory)
        args (into-array Object args)]
    (try
      (.call fun ctx scope nil args)
      (finally (Context/exit)))))

(defn call-timeout [scope fun timeout-ms & args]
  (let [factory (TimedContextFactory. timeout-ms)
        ctx (.enterContext factory)
        args (into-array Object (map #(to-js % scope ctx) args))]
    (try
      (.call fun ctx scope nil args)
      (finally (Context/exit)))))

(defn undefined? [value]
  "return true if value is undefined"
  (= value (. UniqueTag NOT_FOUND)))

(def defined? (complement undefined?))

(defn set! [scope name value]
  "bind an object to a name in scope"
  (.put scope name scope value))

(defn get 
  "return the object referenced by var-name in scope,
  UniqueTag.NOT_FOUND if not found or not-found if supplied"
  ([scope var-name]
    ; TODO: return undefined when scope doesn't have .get
    (.get scope (name var-name) scope))
  ([scope var-name not-found]
    (let [result (.get scope (name var-name) scope)]
      (if (undefined? result)
        not-found
        result))))

(defn get-in
  "Returns the value in a nested scope,
  where ks is a sequence of keys. Returns nil if the key is not present,
  or the not-found value if supplied."
  ([scope ks]
     (reduce get scope ks))
  ([scope ks not-found]
     (loop [sentinel (Object.)
            scope scope
            ks (seq ks)]
       (if ks
         (let [scope (get scope (first ks) sentinel)]
           (if (identical? sentinel scope)
             not-found
             (recur sentinel scope (next ks))))
         scope))))

(defn new-root-scope [& [ctx sealed vars-to-remove]]
  "create a new root js scope and return it
  make it sealed if sealed is true
  remove vars-to-remove if non nil (a seq of strings)"
  (with-context-if-nil ctx
    (fn [ctx]
      (let [scope (.initStandardObjects ctx nil true)]
        ; force loading RegExp
        (eval scope "RegExp;" :ctx ctx)

        (dorun (map #(.delete scope %) (or vars-to-remove [])))

        (when sealed
          (.sealObject scope))

        scope))))

(defn new-safe-root-scope [& [ctx]]
  "create a new root js scope removing dangerous references and sealing it"
  (new-root-scope ctx true insecure-vars))

(defn new-scope [& [ctx parent-scope vars-to-remove]]
  "create a new scope with parent-scope as parent, if parent-scope is nil
  create it"
  (with-context-if-nil ctx
    (fn [ctx]
      (let [parent-scope (or parent-scope (new-root-scope ctx true vars-to-remove))
            scope (.newObject ctx parent-scope)]

        (doto scope
          (.setPrototype parent-scope)
          (.setParentScope nil))

        scope))))

(defn new-safe-scope [& [ctx]]
  "create a new scope using a safe root scope as parent"
  (new-scope ctx (new-safe-root-scope ctx)))

(defn compile-function [scope code & {:keys [filename line-number sec-domain]}]
  "compile and return function defined in code"
  (let [ctx (.enterContext (TimedContextFactory. 0))]
    (try
      (.compileFunction ctx scope code 
                        (or filename "<eval>")
                        (or line-number 1) sec-domain)
      (finally (Context/exit)))))


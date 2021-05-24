(ns fr.jeremyschoffen.prose.alpha.document.sci.bindings)

;;----------------------------------------------------------------------------------------------------------------------
;; Sci namespace bindings helpers
;;----------------------------------------------------------------------------------------------------------------------
(defn macro?
  "True if the var `v` references a macro."
  [v]
  (some-> v
          meta
          :macro))


(defn var->binding
  "De-reference a var and add the `sci/macro` metadata if needed."
  [v]
  (-> v
      deref
      (cond-> (macro? v) (vary-meta assoc :sci/macro true))))


(defn publics->bindings
  "Make a sci bindings map from the result of a `ns-publics` result.

  The vars are de-referenced and in the case of macros the `sci/macro` metadata is added."
  [m]
  (into {}
        (map (juxt key (comp var->binding val)))
        m))


(defmacro bindings
  "Extract bindings using `ns-publics` and using [[publics->bindings]] the result.

  The vars returned by `ns-publics` (the map's values) are de-referenced, in the case of macros
  the `sci/macro` metadata is added."
  [n]
  `(publics->bindings (ns-publics '~n)))


(defmacro make-ns-bindings
  "Make a namespaces bindings map.
  Typically used as `(sci/init {:namespaces (make-ns-bindings ns1 ns2)})`."
  [& nss]
  (into {}
        (map (juxt #(list 'quote %)
                   #(list `bindings %)))
        nss))


(comment
  (macroexpand-1 '(make-ns-bindings fr.jeremyschoffen.textp.alpha.lib.core))
  (make-ns-bindings fr.jeremyschoffen.prose.alpha.lib.core))

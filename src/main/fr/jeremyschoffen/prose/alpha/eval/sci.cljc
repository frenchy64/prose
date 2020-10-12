(ns fr.jeremyschoffen.prose.alpha.eval.sci
  (:require
    [medley.core :as medley]
    [sci.core :as sci :include-macros true]
    [fr.jeremyschoffen.prose.alpha.eval.common :as eval-common]))




(def sci-opt-features {:features #?(:clj #{:clj}
                                    :cljs #{:cljs}
                                    :default #{})})


(def sci-opt-println {:namespaces #?(:clj {}
                                     :cljs {'clojure.core {'println println}})})

(defn init
  "Create a sci evaluation context.

  Same as [[sci.core/init]] with the [[eval-ns]] installed. The goal is to provide code executed by sci
  an environment that has a namespace equivalent to [[fr.jeremyschoffen.prose.alpha.eval.clojure]] pre-installed
  in the namespace `fr.jeremyschoffen.prose.alpha.eval.sci`."
  [opts]
  (->> opts
       (medley/deep-merge sci-opt-features)
       sci/init))


(comment
  (def env (init sci-opt-println))

  (sci/binding [sci/ns @sci/ns
                sci/out *out*]
    (doseq [f '[(println *ns*)
                (ns foobar)
                (def inc* inc)
                (println (inc* 3))
                (println *ns*)]]
      (sci/eval-form env f)))

  (sci/binding [sci/ns @sci/ns
                sci/out *out*]
    (sci/eval-form env
      '(contains? (into (sorted-set)
                        (comp
                          (map str)
                          (map keyword))
                        (all-ns))
                  :foobar))))

;;----------------------------------------------------------------------------------------------------------------------
;; Utilities
;;----------------------------------------------------------------------------------------------------------------------
(defn sci-ctxt->sci-eval
  "Make an eval function from an sci context.

  The result is a function of one argument, a form to be evaluated by sci in the evaluation context `ctxt`."
  [ctxt]
  (fn [form]
    (sci/eval-form ctxt form)))


(defn wrap-sci-bindings
  "Middle for evaluation functions that return an performing the evaluation with the sci `bindings` properly set."
  [eval-fn bindings]
  (fn [form]
    (sci/with-bindings bindings
      (eval-fn form))))

;;----------------------------------------------------------------------------------------------------------------------
;; Eval functions
;;----------------------------------------------------------------------------------------------------------------------
(defn eval-forms
  "Evaluate a sequence of forms with sci ensuring the the current namespace doesn't change after the evaluation."
  ([forms]
   (eval-forms (init nil) forms))
  ([sci-ctxt forms]
   (let [ef (sci-ctxt->sci-eval sci-ctxt)]
     (eval-common/bind-env {:prose.alpha/env :clojure-sci}
       (sci/binding [sci/ns @sci/ns]
         (eval-common/eval-forms ef forms))))))


(comment
  (sci/binding [sci/out *out*]
    (eval-forms
      (init sci-opt-println)
      '[(println *ns*)
        (ns foobar)
        (def inc* inc)
        (inc* 3)
        (println *ns*)]))

  (sci/binding [sci/out *out*]
    (eval-forms '[(println *ns*)
                  (ns foobar)
                  (def inc* inc)
                  (inc* 3)
                  (println *ns*)])))


(defn eval-forms-in-temp-ns
  "Evaluate a sequence of forms with sci in a temporary namespace."
  ([forms]
   (eval-forms-in-temp-ns (init nil) forms))
  ([sci-ctxt forms]
   (let [ef (sci-ctxt->sci-eval sci-ctxt)]
     (eval-common/bind-env {:prose.alpha/env :clojure-sci}
       (sci/binding [sci/ns @sci/ns]
         (eval-common/eval-forms-in-temp-ns ef forms))))))

(comment
  (sci/binding [sci/out *out*]
    (eval-forms-in-temp-ns
      (init sci-opt-println)
      '[(+ 1 2 3)
        (println *ns*)
        (throw (ex-info "some msg" {:toto 1}))]))

  (-> *e ex-data) ;; should contains faulty form
  (-> *e ex-cause  ex-message)
  (-> *e ex-cause ex-cause ex-data))
(ns fr.jeremyschoffen.prose.alpha.reader.core.error
  (:require
    [net.cgrand.macrovich :as macro :include-macros true]
    [fr.jeremyschoffen.prose.alpha.reader.grammar :as g]))


;; TODO: See how it might be possible when displaying an error to show the starting position of a grammatical rule failing instead of just the end.
;; TODO: try and use edamame to get more precise as to where a reding error occurred
(def error-msgs
  {g/special-char
   "the special character"

   g/pipe-char
   "the pipe character"

   g/plain-text
   "plain text without any special character"

   g/escape-char
   "the escape character"

   g/any-char
   "anything... It shouldn't bug on that..."

   g/verbatim-text
   "any text without \\ or \" characters"

   g/clojure-string
   "a clojure string."

   g/clojure-call-text
   "clojure code without string nor embedded code"

   g/symbol-text
   "a clojure symbol here"

   g/tag-spaces
   "only white space between tag-fn argument types"

   g/tag-clj-arg-text
   "clojure code without string nor brackets"

   g/tag-text-arg-text
   "text without embedded code nor braces"})


(defn get-normalized-error-data [e]
  (letfn [(extract-reason [insta-reason]
            (map (fn [{:keys [expecting] :as r}]
                   (case (:tag r)
                     :regexp (get error-msgs expecting r)
                     :string (str "the string \"" expecting "\"")
                     r))
                 insta-reason))
          {:keys [failure] :as ed} (ex-data e)]
    (or
      (when failure
        (case (:type ed)
          ::clojure-reader-error
          (when-some [region (::instaparse.gll/region ed)]
            (-> ed
                (dissoc :region)
                (into region)
                (assoc :reason (ex-message failure))))
          ::grammar-error
          (let [{:keys [index reason line column text]} failure]
            (-> ed
                (assoc :reason (extract-reason reason)
                       :end-index index
                       :end-line line
                       :end-column column
                       :text text)))
          nil))
      (throw e))))


(defn normalize-error [e]
  (ex-info (ex-message e)
           (get-normalized-error-data e)))


(defn print-base-error-msg [e]
  (println (ex-message e))
  (let [{:keys [type reason failure]} (ex-data e)]
    (if (= type ::clojure-reader-error)
      (println reason)
      (do
        (println "The parser expected one of")
        (doseq [x reason]
          (println "- " x))
        (println)
        (println "Instaparse error:")
        (println failure)))))


(defn print-region-position [error]
  (let [{:keys [type
                start-index end-index
                start-line start-column
                end-line end-column]} (ex-data error)]
    (println "Region:")
    (if (= type ::clojure-reader-error)
      (do
        (println "From indexes" start-index "to" end-index)
        (println "line" start-line "column" start-column
                 "to line" end-line "column" end-column))
      (do
        (println "Around index" end-index
                 "line" end-line "column" end-column)))))


(defn print-failed-text [e]
  (println "Failed text:")
  (println (-> e ex-data :text)))


(macro/replace
  #?(:clj {}
     :cljs {*out* *print-fn*
            *err* *print-err-fn*})
  (defn print-error-msg [e]
    (binding [*out* *err*]
      (println "--------------------------------------------------------------------------------")
      (print-base-error-msg e)
      (println "--------------------------------------------------------------------------------")
      (print-region-position e)
      (println "--------------------------------------------------------------------------------")
      (print-failed-text e)
      (println "--------------------------------------------------------------------------------"))))



(defn handle-read-error [e]
  (let [e (normalize-error e)]
    (print-error-msg e)
    (throw e)))

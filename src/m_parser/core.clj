(ns m-parser.core
  (:use [clojure.algo.monads :only [defmonad domonad sequence-m with-monad state-m m-chain]]))

; A parser is a function that takes a string, and returns a list of [:state ; "unparsed remainder"]
; pairs. The resulting list may be empty, to indicate no matches.
;
; The monadic value is a parser.
; The monadic function takes a state & returns a parser.

(defn p-result [state]
  (fn [string]
    (list [state string])))

(defn p-zero [string]
  (list))

(defn p-bind [mv mf]
  (fn [string]
    (let [last-matches (mv string)]
      (mapcat (fn [[last-state last-string]]
                ((mf last-state) last-string)) last-matches))))

(defn p-plus [& parsers]
  (fn [string]
    (mapcat #(% string) parsers)))

(defn p-det [& parsers]
  (fn [string]
    (let [combination (apply p-plus parsers)
          result (combination string)]
      (take 1 result))))

(defn p-optional [parser state]
  (fn [string]
    (let [result (parser string)]
      (if (empty? result)
        (list [state string])
        result))))

(defmonad parser-m [m-result p-result
                    m-bind p-bind
                    m-zero p-zero
                    m-plus p-plus])

(defn make-char-parser [value chr]
  (fn [string]
    (if (= (first string) chr)
      (list [value (. string (substring 1))])
      (list))))

(defn any-char-parser [string]
  (if-let [c (first string)]
    (list [c (subs string 1)])
    (list)))

(defn make-string-parser [value s]
  (fn [string]
    (if (. string (startsWith s))
      (list [value (. string (substring (count s)))])
      (list))))

(defn p-int [string]
  (if-let [[_ n remainder] (re-find #"^(\d+)(.*)" string)]
    (list [(Integer. n) remainder])
    (list)))

(defn- arg-symbols
  "Returns a list of symbols, '(%1 %2 %3 %4...)"
  []
  (map symbol (map #(symbol (str "%" %)) (rest (range)))))

(defmacro defparser [name bindings action]
  `(def ~name
     (domonad parser-m ~(mapcat list
                                (arg-symbols)
                                (map #(cond
                                        (instance? Character %) `(make-char-parser ~% ~%)
                                        :else %)
                                     bindings))
              ~action)))

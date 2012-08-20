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

(defmonad parser-m [m-result p-result
                    m-bind p-bind
                    m-zero p-zero])

(defn make-char-parser [chr value]
  (fn [string]
    (if (= (first string) chr)
      (list [value (. string (substring 1))])
      (list))))

(defn make-string-parser [s value]
  (fn [string]
    (if (. string (startsWith s))
      (list [value (. string (substring (count s)))])
      (list))))

(defn p-int [string]
  (if-let [[_ n remainder] (re-find #"^(\d+)(.*)" string)]
    (list [(Integer. n) remainder])
    (list)))

(ns m-parser.test.core
  (:use [clojure.test]
        [clojure.algo.monads :only [domonad]]
        [m-parser.core]))

; Test monadic laws:
; These tests aren't great. Instead of comparing that functions are equal, they
; compare that the functions behave equally. Well, I guess that's true of all
; testing...
; (See: http://www.intensivesystems.net/tutorials/monads_101.html for the laws.)

; (m-bind (m-result x) f) is equal to (f x)

(deftest test-monadic-law-1
  (let [f (fn [state]
            (fn [string]
              (list [state string])))
        x :start]
    (is (= ((f x) "Hello")
           ((p-bind (p-result x) f) "Hello"))
        (= ((f x) "Suit")
           ((p-bind (p-result x) f) "Suit")))))

; (m-bind mv m-result) is equal to mv

(deftest test-monadic-law-2
  (let [parser (fn [string]
                 (list [(first string) (rest string)]))]
    (is (= (parser "test")
           ((p-bind parser p-result) "test")
           ))))

; (m-bind (m-bind mv f) g) is equal to (m-bind mv (fn [x] (m-bind (f x) g)))

(deftest test-monadic-law-3
  (let [parser-1 (fn [string]
                   (list [(first string) (rest string)]))
        parser-2 (fn [string]
                   (list [:match-char (rest string)]))
        f (fn [state] parser-1)
        g (fn [state] parser-2)
        first-combination (p-bind (p-bind parser-1 f ) g)
        second-combination (p-bind parser-1 (fn [x] (p-bind (f x ) g)))]
    (is (= (first-combination "test")
           (second-combination "test")))))

; (m-plus mv m-zero) produces mv
; (m-plus m-zero mv) produces mv

(deftest test-m-plus-law
  (let [mv (fn [string]
             (list [:thing string]))]
    (is (= ((p-plus mv p-zero) "test")
           ((p-plus p-zero mv) "test")))))

; Less mathematically-formal tests.

(deftest test-make-char-parser
  (let [p-a (make-char-parser :a \a)
        p-b (make-char-parser :b \b)]
    (is (= (list [:a ""]) (p-a "a")))
    (is (= (list [:b ""]) (p-b "b")))
    (is (= (list [:a "bc"]) (p-a "abc")))
    (is (= (list) (p-a "test")))
    (is (= (list) (p-a "")))))

(deftest test-make-string-parser
  (let [p-hello (make-string-parser :hi "hello")]
    (is (= (list [:hi " world"]) (p-hello "hello world")))
    (is (= (list) (p-hello "goodbye world")))))

(deftest test-make-int-parser
  (is (= (list [5 " cats"]) (p-int "5 cats")))
  (is (= (list [76 " trombones"]) (p-int "76 trombones")))
  (is (= (list [76 ".0"]) (p-int "76.0"))))

(deftest test-p-plus-basic
  (let [parser (domonad parser-m [a-or-b (m-plus (make-char-parser :a \a)
                                                 (make-char-parser :b \b))]
                        a-or-b)]
    (is (= (list [:a "bab"]) (parser "abab")))
    (is (= (list [:b "aba"]) (parser "baba")))))

(deftest test-p-plus-ambiguous
  (let [parser (domonad parser-m [a-or-b (m-plus (make-char-parser :a \a)
                                                 any-char-parser)]
                        a-or-b)]
    (is (= (list [:a "bab"] [\a "bab"]) (parser "abab")))
    (is (= (list [\b "aba"]) (parser "baba")))))

(deftest test-p-det
  (let [parser (domonad parser-m [a-or-b (p-det (make-char-parser :a \a)
                                                 any-char-parser)]
                        a-or-b)]
    (is (= (list [:a "bab"]) (parser "abab")))
    (is (= (list [\b "aba"]) (parser "baba")))))

(deftest test-p-optional
  (let [parser (domonad parser-m [a (make-char-parser :a \a)
                                  opt-b (p-optional (make-char-parser :b \b) :noop)
                                  c (make-char-parser :c \c)]
                        [a opt-b c])]
    (is (= (list [[:a :b :c] "def"]) (parser "abcdef")))
    (is (= (list [[:a :noop :c] "e"]) (parser "ace")))))

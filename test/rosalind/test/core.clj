(ns rosalind.test.core
  (:use [rosalind.core])
  (:use [clojure.test]))

(deftest simple-test
  (is (= (hello) "Hello world!"))
  (is (= (hello "test") "Hello test!")))
(ns lisp-maxwell.core-test
  (:require [clojure.test :refer :all]
            [lisp-maxwell.core :refer [evalquote_ eval_ evlis]]))

(deftest a-test
  (testing "Basic tests"
    (is (true? (evalquote_ '(eq (x) (x)))))
    (is (false? (evalquote_ '(eq 1 2))))
    (is (true? (evalquote_ '(eq 1 1))))
    (is (= (evlis '(a b) '((a c) (b 2))) '(c 2)))
    (is (= (eval_ '(car (quote_ (a b))) '(a a b b)) 'a))
    (is (=  (eval_ '(car (quote_ (1 2))) '((1 1) (2 2))) 1))
    (is (= (evalquote_ '((lambda (x) x) a)) 'a))
    (is (= (eval_ '((label f (lambda (x) (car x))) (quote_ (a b))) '((a 1))) 'a))
    (is (true? (eval_ '(eq 1 1) '((1 1)))))))

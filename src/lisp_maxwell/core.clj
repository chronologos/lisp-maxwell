(ns lisp-maxwell.core)

;; https://michaelnielsen.org/ddi/lisp-as-the-maxwells-equations-of-software/
;; Implements core of Lisp with only the following 7 primitives:
;; atom (symbol?)
;; car
;; cdr
;; cons
;; cond
;; eq (=)
;; quote (')

(declare apply_ eval_ evcon evlis pairlis assoc_ cond_ car cdr caar cdar cadr cadar caddr caddar quote_)

;; Transform (cond (p1 e1) (p2 e2) (true e3) ...) to (cond p1 e1 p2 e2 :else e3)
(defmacro cond_ [& pe-list]
  (cons 'cond
        (replace {'true :else}
                 (mapcat list (map first pe-list) (map second pe-list)))))

(defn evalquote_  [q] (apply_ (car q) (cdr q) nil))

(defn apply_
  "Handles a function and its arguments."
  [fn x a]
  (cond
    (symbol? fn)
    (cond (= fn 'car) (caar x)
          (= fn 'cdr) (cdar x)
          (= fn 'cons) (cons (car x) (cadr x))
          (= fn 'atom_) (symbol (car x))
          (= fn 'eq) (= (car x) (cadr x))
          :else (apply_ (eval_ fn a) x a))
    (= (car fn) 'lambda)
    ;; given (lambda param body) arguments env,
    ;; evaluate body of lambda by binding arguments to params in environment.
    (eval_ (caddr fn) (pairlis (cadr fn) x a))
    (= (first fn) 'label)
    ;; label was used in the LISP Manual to give names to procedures
    ;; so that procedure definitions could refer recursively to themselves.
    ;; given (label name body) arguments env,
    ;; apply body to param, with (name body) added to environment
    (apply_ (caddr fn) x (cons
                          (cons (cadr fn)  (caddr fn)) ;; (name body)
                          a))))

(defn eval_
  "Handles forms."
  [e a]
  (cond (or  (symbol? e) (int? e)) (assoc_ e a)
        (symbol? (car e)) (cond (= (car e) 'quote_) (cadr e)
                                (= (car e) 'cond_) (evcon (cdr e) a)
                                :else (apply_ (car e) (evlis (cdr e) a) a))
        :else (apply_ (car e) (evlis (cdr e) a) a)))

;; (cond (p1 e1) (p2 e2))
;; p expressions are evaluated in order till onbe returns t, and the value of the corresponding e expression is returned.
(defn evcon [c a]
  (cond (eval_ (caar c) a) (eval_ (cadar c) a)
        :else (evcon (cdr c) a)))

;; (defn evlis [m a]
;;   (cond_  ((empty? m) '()) (true (cons (eval_ (car m) a) (evlis (cdr m) a)))))

(defn evlis [m a]
  (cond (empty? m) '()
        :else (cons (eval_ (car m) a) (evlis (cdr m) a))))

(defn car [x] (first x))
(defn cdr [x] (rest x))
(defn caar [x] (car (car x)))
(defn cdar [x] (cdr (car x)))
(defn cadr [x] (car (cdr x)))
(defn cadar [x] (car (cdr (car x))))
(defn caddr [x] (car (cdr (cdr x))))
(defn caddar [x] (car (cdr (cdr (car x)))))

(defn pairlis
  ([a b] (map list a b)) ;; kind of cheating, but we know how to do it the hard way too.
  ([a b c] (conj (pairlis a b) c)) ;; TODO: order?
  )

;; use it like: (assoc_ 'y '((a b) (y x))) => x
(defn assoc_ [x y]
  (cond (= (caar y) x) (cadar y)
        (empty? y) '()
        :else (assoc_ x (cdr y))))

(evalquote_ '(eq (x) (x)))
(evalquote_ '(eq 1 2))
(evalquote_ '(eq 1 1))
(evlis '(a b) '((a c) (b 2)))
(eval_ '(car (quote_ (a b))) '(a a b b))
(assert (=  (eval_ '(car (quote_ (1 2))) '((1 1) (2 2))) 1))
(evalquote_ '((lambda (x) x) a))
(eval_ '((label f (lambda (x) (car x))) (quote_ (a b))) '((a 1)))
(eval_ '(eq 1 1) '((1 1)))
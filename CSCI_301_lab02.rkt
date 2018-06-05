#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;;
;; Lab #2
;;
;; Nick Amundsen
;; W01323151
;;
;; The purpose of this program is to
;; calculate Integrals using the summation method
;; along with the D procedure for calculating
;; derivatives from the notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide I)
(provide D)

(define D
  (lambda (f)
    (let* ((delta 0.00001)
           (two-delta (* 2 delta)))
      (lambda (x)
        (/ (- (f (+ x delta))
              (f (- x delta)))
           two-delta)))))

(define Idef-sum (lambda (a b f delta)
  (if (> (+ a delta) b)
      0
      (+ (* (f (+ a delta)) (* (/ (- b a) 100000))) (Idef-sum a b f (+ delta (* (/ (- b a) 100000))))))))

(define Idef (lambda (a b f)
  (let ((delta (* (/ (- b a) 100000))))
      (Idef-sum a b f delta))))

(define I (lambda (f)
            (lambda (a b) (- (Idef 0 b f) (Idef 0 a f)))))



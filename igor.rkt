#lang racket

(require (for-syntax racket))
(provide (all-defined-out))

(struct message (value sender))

(define (send-message routine value)
  (thread-send routine (message value (current-thread))))

(define-syntax (define-routine stx)
  (define top-form (syntax->datum stx))
  (match-define (list _ name-id state-constructor-id message-value-id state-id alarm-id sender-id) (take top-form 7))
  (define (compile-send-form send-form)
    `(send-message ,sender-id ,(second send-form)))
  (define (compile-recurse-form recurse-form)
    (match (length recurse-form)
      [1 `(recurse-procedure ,state-id ,alarm-id)]
      [2 `(recurse-procedure ,(second recurse-form) ,alarm-id)]
      [3 `(recurse-procedure ,(second recurse-form) ,(third recurse-form))])) ;; Should give useful error message if wrong form length.
  (define (compile-on-form-with-send-and-recurse forms)
    (define match-value (second forms))
    (define sub-match-value (third forms))
    (define body (drop forms 3))
    (define send-forms (filter (compose (curry eq? 'send) first)
                               body))
    (define recurse-forms (filter (compose (curry eq? 'recurse) first)
                                  body))
    `[,match-value
      (match-let ([,sub-match-value ,message-value-id])
        ,@(append (map compile-send-form send-forms)
                  (map compile-recurse-form recurse-forms)))])
  (define body (drop top-form 7)) ;; All forms should be pair? , all forms should start with 'recurse or 'send ?
  (define on-forms (filter (compose (curry eq? 'on)
                                    first)
                           body))
  (define on-alarm-forms (filter (compose (curry eq? 'on-alarm) first)
                                 body))
  (define compiled-body `(match ,alarm-id
                           ,@(map compile-on-form-with-send-and-recurse on-alarm-forms)
                           [null (match ,message-value-id
                                ,@(map compile-on-form-with-send-and-recurse on-forms)
                                [_ (recurse-procedure ,state-id 'no-matching-clause)])]
                           [_ (recurse-procedure ,state-id 'no-matching-alarm-clause)]))
  (datum->syntax stx
                 `(define (,name-id initial-state)
                    (define (recurse-procedure ,state-id ,alarm-id)
                      (match-define (message ,message-value-id ,sender-id) (thread-receive))
                      ,compiled-body)
                    (thread (lambda ()
                              (recurse-procedure initial-state null))))))
                            
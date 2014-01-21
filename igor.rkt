#lang racket

(require (for-syntax racket))
(provide (all-defined-out))

(struct message (value sender))

(define (send-message routine value)
  (thread-send routine (message value (current-thread))))

(define-syntax (define-routine stx)
  (define top-form (syntax->datum stx))
  (match-define (list _ name-id state-constructor-id message-value-id state-id alarm-id sender-id) (take top-form 7))
  (define (compile-on-form-with-tell-and-recurse forms)
    (define match-value (second forms))
    (define body (drop forms 2))
    `[,match-value (match ,message-value-id
                     ,@body)])
  (define body (drop top-form 7)) ;; All forms should be pair? , all forms should start with 'recurse or 'send ?
  (define on-forms (filter (compose (curry eq? 'on)
                                    first)
                           body))
  (define on-alarm-forms (filter (compose (curry eq? 'on-alarm) first)
                                 body))
  (define compiled-body `(match ,alarm-id
                           ,@(map compile-on-form-with-tell-and-recurse on-alarm-forms)
                           [null (match ,message-value-id
                                   ,@(map compile-on-form-with-tell-and-recurse on-forms)
                                   [_ (recurse-procedure ,state-id 'no-matching-clause)])]
                           [_ (recurse-procedure ,state-id 'no-matching-alarm-clause)]))
  (datum->syntax stx
                 `(define (,name-id initial-state)
                    (define (recurse-procedure ,state-id ,alarm-id)
                      (define (tell value [receiver ,sender-id])
                        (thread-send receiver (message value (current-thread))))
                      (define (recurse [new-state ,state-id] [new-alarm ,alarm-id])
                        (recurse-procedure new-state new-alarm))
                      (match-define (message ,message-value-id ,sender-id) (thread-receive))
                      ,compiled-body)
                    (thread (lambda ()
                              (recurse-procedure initial-state null))))))
                            
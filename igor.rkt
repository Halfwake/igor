#lang racket

(require (for-syntax racket))
(provide (all-defined-out))

(struct message (type value sender))

(define-syntax (define-routine stx)
  (define top-form (syntax->datum stx))
  (match-define (list _ name-id state-constructor-id type-id message-value-id state-id alarm-id sender-id) (take top-form 8))
  (define (compile-on-form-with-tell-and-recurse forms)
    (define match-value (second forms))
    (define body (drop forms 2))
    `[,match-value (match ,message-value-id
                     ,@body)])
  (define body (drop top-form 8)) ;; All forms should be pair? , all forms should start with 'recurse or 'send ?
  (define on-forms (filter (compose (curry eq? 'on)
                                    first)
                           body))
  (define on-alarm-forms (filter (compose (curry eq? 'on-alarm) first)
                                 body))
  (define compiled-body `(match ,alarm-id
                           ,@(map compile-on-form-with-tell-and-recurse on-alarm-forms)
                           [null (match ,type-id
                                   ,@(map compile-on-form-with-tell-and-recurse on-forms)
                                   [_ (recurse-procedure ,state-id 'no-matching-clause)])]
                           [_ (recurse-procedure ,state-id 'no-matching-alarm-clause)]))
  (datum->syntax stx
                 `(define (,name-id initial-state)
                    (define (recurse-procedure ,state-id ,alarm-id)
                      (define (tell type value [receiver ,sender-id])
                        (thread-send receiver (message type value (current-thread))))
                      (define (recurse [new-state ,state-id] [new-alarm ,alarm-id])
                        (recurse-procedure new-state new-alarm))
                      (match-define (message ,type-id ,message-value-id ,sender-id) (thread-receive))
                      ,compiled-body)
                    (thread (lambda ()
                              (recurse-procedure initial-state null))))))
                            
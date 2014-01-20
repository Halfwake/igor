#lang racket

(require "igor.rkt")

(struct fridge (door-open food-amount))

(define (fridge-close old-fridge)
  (struct-copy fridge old-fridge [door-open #f]))

(define (fridge-open old-fridge)
  (struct-copy fridge old-fridge [door-open #t]))

(define (fridge-open? fridge)
  (fridge-door-open fridge))

(define-routine fridge-routine fridge message-value state alarm sender
  (on-alarm 'bad-sender-value _
    (recurse state null))
  (on 'open _
    (recurse (fridge-open state)))
  (on 'close _
    (recurse (fridge-close state)))
  (on 'open? _
    (send (fridge-open? state))
    (recurse state)))

(define f (fridge-routine (fridge #f 0)))
(send-message f 'open?)
(message-value (thread-receive))
(send-message f 'open)
(send-message f 'open?)
(message-value (thread-receive))
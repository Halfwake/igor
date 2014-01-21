#lang racket

(require "igor.rkt")

(struct fridge (door-open food-amount))

(define (fridge-close old-fridge)
  (struct-copy fridge old-fridge [door-open #f]))

(define (fridge-open old-fridge)
  (struct-copy fridge old-fridge [door-open #t]))

(define (fridge-open? fridge)
  (fridge-door-open fridge))

(define-routine fridge-routine fridge type value state alarm sender
  (on-alarm 'bad-sender-value
    [_
     (recurse state null)])
  (on 'open
    [_
     (recurse (fridge-open state))])
  (on 'close
    [_
     (recurse (fridge-close state))])
  (on 'open?
    [_ 
     (tell 'ok (fridge-open? state))
     (recurse)]))

(define f (fridge-routine (fridge #f 0)))
(thread-send f (message 'open? null (current-thread)))
(message-value (thread-receive))
(thread-send f (message 'open null (current-thread)))
(thread-send f (message 'open? null (current-thread)))
(message-value (thread-receive))
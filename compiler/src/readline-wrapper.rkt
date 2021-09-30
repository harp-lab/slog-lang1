#lang racket

(require "utils.rkt")

(provide install-readline!
         readline
         set-completion-function!)

(begin-for-syntax
  (define windows? (equal? (system-type 'os) 'windows)))

(define (win-readline prompt)
    (display prompt)
    (read-line))

(%if windows?
  (begin
    (define install-readline! void)
    (define set-completion-function! void)
    (define readline win-readline)) 
  (begin
    (require (prefix-in rl: readline))
    (require (prefix-in rl: readline/pread))
    (require (prefix-in rl: readline/readline))
    (define install-readline! rl:install-readline!)
    (define set-completion-function! rl:set-completion-function!)
    (define readline rl:readline)))

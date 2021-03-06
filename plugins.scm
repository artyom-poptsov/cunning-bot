;; This file is part of Cunning Bot, an IRC bot written in Guile Scheme.
;; Copyright (C) 2013 Ian Price

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (cunning-bot plugins)
  #:use-module (srfi srfi-9)
  #:use-module ((srfi srfi-1) #:select (alist-cons remove))
  #:use-module (cunning-bot bot)
  #:export (use-plugin!
            use-plugins!
            remove-plugin!
            ))

(define (module-unuse! module interface)
  (cond ((member interface (module-uses module))
         (set-module-uses! module (remove (lambda (x) (eqv? x interface)) (module-uses module)))
         (hash-clear! (module-import-obarray module)) ; necessary?
         (module-modified module))
        (else
         (throw 'something))))

(define-record-type plugin
  (%make-plugin module interface setup teardown)
  plugin?
  (module plugin-module)
  (interface plugin-interface)
  (setup plugin-setup-procedure)
  (teardown plugin-teardown-procedure))

(define (lookup-plugin plugin-name)
  (define module
    (resolve-module `(cunning-bot plugins ,plugin-name)))
  (define hidden-exports
    (filter (lambda (f)
              (module-defined? module f))
            magic-exports))
  (define public-interface
    (resolve-interface `(cunning-bot plugins ,plugin-name) #:hide hidden-exports))
  (define setup! (module-ref module 'setup! #f))
  (define teardown! (module-ref module 'teardown! #f))
  (%make-plugin module public-interface setup! teardown!))

;; Setup should take additional arguments
(define magic-exports '(setup! teardown!))

(define (use-plugin! bot plugin-name)
  "Loads plugin (cunning-bot plugins PLUGIN-NAME) into BOT.

If PLUGIN-NAME is a symbol, it is the name of the plugin to
load. If it is a list, then the car is the name, and the cdr is the
arguments to give to the setup! procedure of the plugin."
  ;; TODO: check plugin not already loaded
  (define setup-args
    (if (pair? plugin-name) (cdr plugin-name) '()))
  (define plugin-name*
    (if (pair? plugin-name) (car plugin-name) plugin-name))
  (define plugin (lookup-plugin plugin-name*))
  (define command-module
    (get-commands bot))
  (module-use! command-module (plugin-interface plugin))
  (and=> (plugin-setup-procedure plugin)
         (lambda (f) (apply f bot setup-args)))
  (and=> (plugin-teardown-procedure plugin)
         (lambda (f) (add-quit-hook! bot f)))
  (bot-plugins-set! bot (alist-cons plugin-name* plugin (bot-plugins bot)))
  *unspecified*)

(define (use-plugins! bot plugins)
  (for-each (lambda (plugin)
              (use-plugin! bot plugin))
            plugins))

(define (remove-plugin! bot plugin-name)
  (define command-module
    (get-commands bot))
  (define plugin
    (cond ((assoc plugin-name (bot-plugins bot)) => cdr)
          (else (throw 'something))))
  (module-unuse! command-module (plugin-interface plugin))
  (let ((teardown! (plugin-teardown-procedure plugin)))
    (when teardown!
      (remove-quit-hook! bot teardown!)
      (teardown! bot)))
  (bot-plugins-set! bot (remove (lambda (x) (eqv? (car x) plugin-name)) (bot-plugins bot)))
  *unspecified*)

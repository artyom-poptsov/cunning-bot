;; This file is part of Cunning Bot, an IRC bot written in Guile Scheme.
;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>

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

(define-module (cunning-bot plugins indicator)
  #:use-module (rpc rpc)
  #:use-module (rpc xdr)
  #:use-module (rpc xdr types)
  #:use-module (indicator rpc-client)
  #:use-module (indicator rpc-types+constants)
  #:export (connect-to-indicator! set-state!))

(define *indicator-socket* #f)

(define (connect-to-indicator! address port)
  "Connect to Indicator RPC server on the given ADDRESS and PORT"
  (let ((server-socket (socket PF_INET SOCK_STREAM 0)))
    (connect server-socket AF_INET (inet-aton address) port)
    (set! *indicator-socket* server-socket)))

(define state->number
  (let ((states `((notice  . 1)
                  (warning . 2))))
    (lambda (state)
      "Convert STATE symbol to a state number"
      (assq state states))))


(define (set-state! bot sender args)
  "Manually set the Indicator state"
  (if *indicator-socket*
      (let ((state (state->number (string->symbol args))))
        (if state
            (INDICATOR-PROGRAM-set-state (cdr state) 123 *indicator-socket*)
            "No such state"))
      "Indicator is not connected"))

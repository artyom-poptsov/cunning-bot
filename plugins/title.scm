;;; title.scm -- Plugin that gets web page titles

;; This file is part of Cunning Bot, an IRC bot written in Guile Scheme.
;;
;; Copyright (C) 2015 Artyom V. Poptsov

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

(define-module (cunning-bot plugins title)
  #:use-module (cunning-bot bot)
  #:use-module (srfi srfi-8)
  #:use-module (sxml simple)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (title))

(define (title bot sender args)
  (let ((uri (string->uri args)))
    (receive (header port)
        (http-get uri #:streaming? #t)
      (let read ((line (read-line port)))
        (if (not (eof-object? line))
            (let ((m (string-match "<title>(.*)</title>" line)))
              (if (regexp-match? m)
                  (format #f "~a: ~a" sender (match:substring m 1))
                  (read (read-line port)))))))))

;;; title.scm ends here

;; This file is part of Cunning Bot, an IRC bot written in Guile Scheme.
;;
;; Copyright (C) 2014 Artyom V. Poptsov

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

(define-module (cunning-bot log)
  #:use-module (ice-9 regex)
  #:export (log-line))

(define %log-file-format-version 1)

(define (make-log-file! file-name)
  (if (file-exists? file-name)
      (open-file file-name "a")
      (let ((p (open-file file-name "a")))
        (format p "# -*- mode: conf-colon; mode: read-only -*-~%")
        (format p "# log-format-version: ~d~%" %log-file-format-version)
        p)))

(define (make-log-port! log-dir ctime channel)
  "Open a log file in LOG-DIR for the CHANNEL and return the opened
port."

  (define (mkdir-if-not-exist! dir)
    "Make directory only if it does not exist"
    (and (not (file-exists? dir))
         (mkdir dir)))

  (let* ((ltime       (localtime ctime))
         (year        (+ 1900 (tm:year ltime)))
         (month       (format #f "~2,'0d" (+ 1 (tm:mon  ltime))))
         (channel-dir (format #f "~a/~a" log-dir     channel))
         (year-dir    (format #f "~a/~a" channel-dir year))
         (month-dir   (format #f "~a/~a" year-dir    month)))

    ;; Make the hierarchy of directories
    (for-each (lambda (dir) (mkdir-if-not-exist! dir))
              (list channel-dir year-dir month-dir))

    (let ((ftime (strftime "%F" (localtime ctime))))
      (make-log-file! (format #f "~a/~a.log" month-dir ftime)))))

(define (log-line log-dir line)
  "Parse the LINE and write it in a log file."
  ;; (format #t "*** ~a~%" line)           ;DEBUG
  (let ((ctime (current-time)))
    (cond

     ((string-match "^:(.*)!.*@.* PRIVMSG ([^:]*) :(.*)" line) =>
      (lambda (match)
        (let* ((sender  (match:substring match 1))
               (channel (match:substring match 2))
               (message (match:substring match 3))
               (p       (make-log-port! log-dir ctime channel)))
          (format p "~a:~a:PRIVMSG:~a~%" ctime sender message)
          (close p))))

     ((string-match "^:(.*)!.*@.* ([A-Z]*) ([^:]*)" line) =>
      (lambda (match)
        (let* ((sender  (match:substring match 1))
               (command (match:substring match 2))
               (channel (match:substring match 3))
               (p       (make-log-port! log-dir ctime channel)))
          (format p "~a:~a:~a~%" ctime sender command)
          (close p)))))))

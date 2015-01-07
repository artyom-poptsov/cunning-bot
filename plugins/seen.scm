;;; seen.scm -- When a nickname was seen last time?

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

(define-module (cunning-bot plugins seen)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (rnrs sorting)
  #:use-module (dsv)
  #:export (seen
            set-log-dir!))

(define *log-directory* #f)

(define (set-log-dir! log-dir)
  "Set bot log directory to a LOG-DIR."
  (set! *log-directory* log-dir))

;; Taken from GNU Guile manual
(define remove-stat
  ;; Remove the `stat' object the `file-system-tree' provides
  ;; for each file in the tree.
  (match-lambda
   ((name stat)              ; flat file
    name)
   ((name stat children ...) ; directory
    (list name (map remove-stat children)))))

(define (get-channels log-dir)
  "Get channels from a LOG-DIR "
  (cadr (remove-stat (file-system-tree log-dir))))

(define (remove-#s channel-name)
  "Remove '#' symbols from a CHANNEL-NAME."
  (regexp-substitute/global #f "#*" channel-name 'pre "" 'post))

(define (grep-file nick log-file)
  "Grep a LOG-FILE for the last record related to a NICK."
  (let ((dsv (reverse (dsv-read (open-input-file log-file)))))
    (find (lambda (record)
            (string=? (cadr record) nick))
          dsv)))

(define (find-fold proc lst)
  "Find the first element of a list LST for which PROC returns a
non-#f value, and return that value."
  (let ff ((l lst))
    (and (not (null? l))
         (let ((result (proc (car l))))
           (if result
               result
               (ff (cdr l)))))))

(define (search-logs log-dir nick)
  "Search logs in LOG-DIR for the last record related to a NICK."

  (define (sort-dirs dirs)
    (list-sort (lambda (e1 e2) (string>? (car e1) (car e2))) dirs))

  (define (sort-files files)
    (list-sort (lambda (e1 e2) (string>? e1 e2)) files))

  (find-fold
   (lambda (channel-dir)
     (find-fold
      (lambda (year-dir)
        (find-fold
         (lambda (month-dir)
           (find-fold
            (lambda (log-file)
              (grep-file nick
                         (string-append
                          log-dir "/"
                          (car channel-dir) "/"
                          (car year-dir) "/"
                          (car month-dir) "/"
                          log-file)))
            (sort-files (cadr month-dir))))
         (sort-dirs (cadr year-dir))))
      (sort-dirs (cadr channel-dir))))
   (get-channels log-dir)))

(define (log-record->event record)
  "Convert a log RECORD to an event string."
  (let ((nick      (cadr record))
        (action    (caddr record))
        (timestamp (string->number (car record))))
  (format #f "~a was last seen ~a~a"
          nick
          (date->string (time-utc->date (make-time 'time-utc 0 timestamp))
                        "~4")
          (cond
           ((string=? action "PRIVMSG")
            (let ((message (cadddr record)))
              (format #f ", saying \"~a\"" message)))
           ((string=? action "JOIN")
            (format #f ", joining the channel"))
           ((string=? action "PART")
            (format #f ", leaving the channel"))
           (else "")))))

(define (seen bot sender args)
  (let ((nicknames (string-split args #\space)))
    (cond
     ((and (= (length nicknames) 1)
           (string-null? (car nicknames)))
      (format #f "~a: Seen who?  Please give me a nickname." sender))
     ((= (length nicknames) 1)
      (let ((record (search-logs *log-directory* (car nicknames))))
        (format #f "~a: ~a"
                sender
                (if record
                    (log-record->event record)
                    "Nope!"))))
     (else
      (format #f "~a: Please give me one nickname at the time." sender)))))

;;; seen.scm ends here

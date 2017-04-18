#!/usr/bin/env gosh
(use srfi-1)
(use srfi-13)


(define (replace-string line . aux)
  #|(replace-string "test1 test2 test3"
                  "test" "sample"
                  "1" "one" "3" "three")|#
  (letrec ((rec (lambda (line from to rest)
                  (if (null? rest)
                      (regexp-replace-all (string->regexp from) line to)
                      (rec (regexp-replace-all (string->regexp from) line to)
                           (car rest) (cadr rest) (cddr rest))))))
    (rec line (car aux) (cadr aux) (cddr aux))))



(define (backslash->lf str)
  (regexp-replace-all (string->regexp "\\\\") str "\n"))


(define (extract-from-file file)
  (let ((read-flg #f)
        (acc '()))
    (with-input-from-file file
      (lambda ()
        (port-for-each
         (lambda (line)
           (cond ((= (string-length line) 0) (set! read-flg #f))
                 (read-flg (push! acc (string-trim-both line)))
                 ((and (>= (string-length line) 23)
                       (string=? (substring line 0 23) " Test job not archived."))
                  (set! read-flg #t))
                 
                 ))
         read-line)))
    acc))

(define (main args)
  (let1 inpfile (second args)
    (print
     "%MEM=6GB\n"
     (replace-string
      (backslash->lf
       (((string->regexp "Version=")
         (((string->regexp "^.*?\\\\\\\\")
           (fold string-append "" (extract-from-file inpfile)))
          'after))
        'before)
       )
      "gfprint" "gfinput Opt=(Z-matrix, maxstep=10) freq"
      ))))

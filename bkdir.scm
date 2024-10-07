(import (chicken foreign))
(import (chicken string))
(import (chicken sort))
(import (chicken io))
(import (chicken format))
(import (chicken file))
(import (chicken port)
        (chicken process-context))
(import foreigners)
(import srfi-1)

#>
        #define TB_IMPL
        #include "termbox2.h"
<#

(define TB-EVENT-KEY 1)
(define TB-EVENT-RESIZE 2)
(define TB-EVENT-MOUSE 3)
(define TB-REVERSE 1024)

(define TB-KEY-BACKSPACE 127)
(define TB-KEY-ARROW-DOWN 65516)
(define TB-KEY-ARROW-UP 65517)
(define TB-KEY-CONTROL-Q 17)
(define TB-KEY-ENTER 13)

(define tb-init (foreign-lambda int "tb_init"))
(define tb-shutdown (foreign-lambda int "tb_shutdown"))
(define tb-clear (foreign-lambda int "tb_clear"))
(define tb-poll-event (foreign-lambda int "tb_poll_event" (c-pointer (struct "tb_event"))))
(define tb-event*
  (foreign-lambda*
    (c-pointer (struct "tb_event"))
    ()
    "struct tb_event e;
     C_return(&e);"))
(define tb-print (foreign-lambda int "tb_print" int int unsigned-int unsigned-int c-string))
(define tb-present (foreign-lambda int "tb_present"))

(define-foreign-record-type (tb-event "struct tb_event")
  (unsigned-byte type tb-event-type tb-event-type!)
  (unsigned-byte mod tb-event-mod tb-event-mod!)
  (unsigned-short key tb-event-key tb-event-key!)
  (unsigned-int32 ch tb-event-ch tb-event-ch!)
  (int32 w tb-event-w tb-event-w!)
  (int32 h tb-event-h tb-event-h!)
  (int32 x tb-event-x tb-event-x!)
  (int32 y tb-event-y tb-event-y!))

(define ep (tb-event*))

(define bookmarks-dir
  (let ((dir (conc (or (get-environment-variable "XDG_DATA_HOME") 
                       (conc (get-environment-variable "HOME") "/.local/share")) "/bkdir")))
  (create-directory dir #t)
  (conc dir "/bookmarks")))

(define lista 
  (if (file-exists? bookmarks-dir)
    (call-with-input-file bookmarks-dir
                          (lambda (port)
                            (read-list port)))
    (list)))

(define (bookmark)
  (current-directory)
  (with-output-to-file bookmarks-dir
    (lambda ()
      (write (current-directory))
      (newline))
    #:append)
  (display "Saved directory\n"))

(define (print-list l y i)
  (let ((counter 0))
   (for-each 
      (lambda (x)
        (if (= counter (remainder i 5))
            (tb-print 0 y 0 TB-REVERSE x)
            (tb-print 0 y 0 0 x))
        (set! counter (+ counter 1))
        (set! y (+ y 1)))
      (let* ((l1 (drop l (* (quotient i 5) 5)))
	     (l2 (take l1 (min 5 (length l1)))))
	l2)
      )))

(define (lcs-distance s t)
  (let* ((m (string-length s))
        (n (string-length t))
        (vec (make-vector (+ m 1) 0))
        (cur-max 0))
    (begin
      (do ((i 0 (add1 i)))
          ((= i (+ m 1)))
          (vector-set! vec i (make-vector (+ n 1) 0)))

      (do ((i 1 (add1 i)))
          ((= i (+ m 1)))
          (do ((j 1 (add1 j)))
              ((= j (+ n 1)))
              (when (char=? (string-ref s (- i 1)) (string-ref t (- j 1)))
                (begin
                  (vector-set! (vector-ref vec i) j
                               (+ (vector-ref (vector-ref vec (- i 1)) (- j 1)) 1))
                  (when (> (vector-ref (vector-ref vec i) j) cur-max)
                    (set! cur-max (vector-ref (vector-ref vec i) j)))))))
      cur-max)))

(define (enter s)
  (tb-shutdown)
  (display s))

(define (print-ui s i)
    (tb-clear)
    (tb-print 0 2 0 0 ">")
    (tb-print 2 2 0 0 s)
    
    (if (= (length lista) 0)
        (begin
          (tb-print 0 4 0 0  "No saved bookmarks. To save a bookmark use \"bkdir b\".")
          (tb-print 0 6 0 0  "Exit with CTRL-Q."))
        (begin
          (tb-print 0 11 0 0 (->string i))
          (print-list (sort lista (lambda (x y) (> (lcs-distance x s) (lcs-distance y s))))
                      4 i)))
    (tb-present))

(define (lop s i)
  (print-ui s i)
  (tb-poll-event ep)

  (let ((key (tb-event-key ep))
	(new-s
          (cond 
            ((= (tb-event-key ep) TB-KEY-BACKSPACE)
	     (begin
	       (set! i 0)
	       (substring s 0 (max (- (string-length s) 1) 0))))
            ((not (= (tb-event-ch ep) 0))
	     (begin
	       (set! i 0)
	       (conc s (integer->char (tb-event-ch ep)))))
            (else s)))
        (new-i
          (cond 
            ((= (tb-event-key ep) TB-KEY-ARROW-DOWN)
             (min (+ i 1) (- (length lista) 1)))
            ((= (tb-event-key ep) TB-KEY-ARROW-UP)
             (max (- i 1) 0))
            (else i))))
    (cond
      ((= key TB-KEY-ENTER) (if (= (length lista) 0)
                                (tb-shutdown)
                                (enter
                                  (list-ref 
                                    (sort lista (lambda (x y) (> (lcs-distance x new-s) (lcs-distance y new-s))))
                                    new-i))))
      ((= key TB-KEY-CONTROL-Q) (tb-shutdown))
      (else (lop new-s new-i)))))


(define (usage)
  (display "USAGE: bkdir [b]\nCalling with b bookmarks current working directory\nCalling without any arguments launches bookmar manager\n"))

(cond
  ((equal? (command-line-arguments) (list "b")) (bookmark))
  ((equal? (command-line-arguments) '()) (begin 
                                           (tb-init)
                                           (lop "" 0)))
  (else (usage)))

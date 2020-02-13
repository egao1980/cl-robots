(defpackage cl-robots
  (:use :cl
        :esrap)
  (:import-from #:cl-unicode 
                #:has-property)
  (:export #:read-robots.txt
           #:sitemaps))
(in-package :cl-robots)

(defrule ws 
    (or #\Space #\Tab)
  (:constant nil))

(defrule nl 
    (or #\Newline #\Return (and #\Return #\Newline))
  (:constant nil))

(defun unicode-control-character-p (ch)
  (has-property ch "Cc"))

(defrule utf8-char-noctl
    (not (unicode-control-character-p character)))

(defrule comment
    (and #\# (* (or utf8-char-noctl ws #\#)))
  (:constant nil))

(defrule eol
    (and (* ws) (? comment) nl) 
  (:constant nil))


(defrule emptyline 
    eol
  (:constant nil))

(defrule identifier
    (+ (character-ranges #\- (#\A #\Z) #\_ (#\a #\z)))
  (:text t))


(defrule disallow-verb 
    (and  (or (~ "disallow")
              (~ "dissallow")
              (~ "dissalow")
              (~ "disalow")
              (~ "diasllow")
              (~ "disallaw")) 
          (* identifier))
  (:constant "disallow"))


(defrule verb
  (or disallow-verb identifier))

(defrule empty-pattern 
    (* ws)
  (:constant nil))

(defrule path-pattern
    (or 
     (and #\/ (* utf8-char-noctl))
     (and #\* (+ utf8-char-noctl)))
  (:text t))

(defrule product-token
    (or identifier #\*))

(defrule rule 
    (and (* ws) verb (* ws) #\: (* ws) (or path-pattern empty-pattern) eol)
  (:destructure (w1 verb w2 col w3 path e)
   (declare (ignore w1 w2 w3 col w3 e))
   (cons (intern (string-downcase (text verb))) (text path))))

(defrule startgroupline
    (and (* ws) (or (~ "user agent") (~ "user-agent")) (* ws) #\: (* ws) product-token eol)
  (:destructure (w1 agent w2 col w3 token e)
   (declare (ignore w1 agent w2 col w3 e))
   (cons (intern "user-agent") (text token))))


(defrule sitemap 
    (and (* ws) (or (~ "site-map") (~ "sitemap")) (* ws) #\: (* ws) (and (~ "http") (? (~ "s")) "://" (+ utf8-char-noctl)) eol)
  (:destructure (w1 site w2 col w3 url e)
   (declare (ignore w1 w2 col w3 e))
   (cons (intern (text site)) (text url))))

(defrule group 
    (and startgroupline
         (* (or startgroupline emptyline))
         (* (or rule emptyline)))
  (:destructure (gr grs rules)
    (list (cons gr (remove nil grs)) (remove nil rules))))

(defrule robotstxt
    (* (or startgroupline rule sitemap emptyline))
  (:lambda (list)
    (remove nil list)))

(defun read-robots.txt (text) 
  (parse 'robotstxt text))

(defun sitemaps (robots)
  (mapcar #'cdr (remove-if-not (lambda (x) (eql (car x) '|sitemap|)) robots)))




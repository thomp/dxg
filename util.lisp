;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DXG; Base: 10 -*-
(in-package :dxg)
;;;
;;; util.lisp: misc utility fns
;;;

;;;
;;; DCU/shared-char
;;; 
(defun whitespacep (char)
  (member char '(#\Newline #\Space #\Tab #\Return #\Linefeed #\Page)))

;;;
;;; DAT-CL-UTILS/shared-string.lisp
;;;
(defun empty-string-p (str)
  (do ((n 0 (1+ n)))
      ((eq n (length str))
       t)
    (if (not (whitespacep (elt str n)))
	(return nil))))

(defun noes (x &optional (no-nils-p t))
  "Return T if X is NIL or if it is an empty string (as defined by EMPTY-STRING-P). If NO-NILS-P is true, return T if X is the string 'NIL'"
  (or (not x)
      (empty-string-p x)
      (if no-nils-p 
	  (if (and (stringp x) (string= x "NIL")) t))))

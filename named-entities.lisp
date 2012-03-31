(in-package :dxg)
;;;
;;; named-entities.lisp: XML 1.0 named entities
;;;
(defun char-to-xml-entity (some-string)
  "Given string SOME-STRING, substitute all characters which are represented by predefined XML 1.0 entities with the corresponding character references."
  (declare (optimize (speed 3))
	   (string some-string))
  ;; CHARS: XML 1.0 specifies the amp, lt, gt, apos, and quot predefined entities
  ;; - with some algorithms, important to have ampersand first (for substitutions which generate &xxx; in documents)
  (let ((chars 
	 '(#\& #\< #\> #\' #\"))
	(entity-strings
	 '("&amp;" "&lt;" "&gt;" "&apos;" "&quot;")))
    (with-output-to-string (s)
      (loop
	 for char across some-string
	 do
	   (let ((pos (position char chars :test #'char=)))
	     (if pos
		 (write-string (nth pos entity-strings) s)
		 (write-char char s)))))))
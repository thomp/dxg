;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DXG; Base: 10 -*-
(in-package :dxg)
;;;
;;; xml-generator.lisp: generate XML strings
;;;

;; (defun attr-string (name value)
;;   (concatenate 'string name "=\"" (coerce-to-string value) "\""))

;; (defun attr-strings (alist)
;;   "ALIST is alist with name/value pairs. If value is nil, pair is discarded. Return a string composed of a='b' pairs."
;;   (let ((return-string ""))
;;     (dolist (name/value alist)
;;       (let ((name (car name/value))
;; 	    (value (cdr name/value)))
;; 	(if value
;; 	    (setf return-string (concatenate 'string return-string " " (attr-string name value))))
;; 	))
;;     ;; trim front space off...
;;     (string-left-trim " " return-string)
;;     ))

(defun attributes-to-xml (attributes)
  "Given a list of lists where each sublist has the form (attribute1 value1), return a string representing the attributes in the form used in an XML document. 

VALUEn can be ... any object?

If VALUEn is NIL, ignore that attribute specification."
  (with-output-to-string (s)
    (do ((apair (pop attributes) (pop attributes)))
	((and (null attributes) (not apair)))
      (let ((a (first apair))
	    (v (second apair)))
	(if v
	    (format s " ~A=\"~A\"" a v ))))))

(defun empty-tag (label &key namespace attributes (attributes-string "") stream)
  "Return a string. ATTRIBUTES is a list of lists of the form ((attribute1 value1) (attribute2 value2) ...). ATTRIBUTES-STRING is a string placed at position where attributes are specified."
  (assert (listp attributes))
  (write-string
   (with-output-to-string (s)
     (write-char #\< s)
     (when (not (noes namespace))
       (write-string namespace s)
       (write-char #\: s))
     (write-string label s)
     (when attributes
       ;;(write-char #\Space s) ;; ATTRIBUTES-TO-XML introduces a space preceding attributes
       (write-string (attributes-to-xml attributes) s))
     (when attributes-string
       (write-char #\Space s)
       (write-string attributes-string s))
     (write-char #\/ s)
     (write-char #\> s))
   stream))

(defun end-tag (label &key namespace stream) ; formerly &optional namespace
  " "
  (let ((string
	 (if (noes namespace)
	     ;; format dangerous w/hunchentoot 1.0?
	     ;; OLD: (format stream "</~A>" label)
	     (concatenate 'string "</" label ">") 
	     ;; OLD: (format stream "</~A:~A>" namespace label)
	     (concatenate 'string "</" namespace ":" label ">")
	     )))
    (if stream (write-string string stream) string)))

(defun start-tag (label &key namespace attributes verbatim-attributes stream) ; formerly &optional namespace
  "ATTRIBUTES is a list of lists of the form ((attribute1 value1) (attribute2 value2) ...) 

If non-NIL, VERBATIM-ATTRIBUTES is a string which should be included verbatim at the attributes position of the tag.

VALUEn can be ?? string? any object? If VALUEn is NIL, ignore that attribute specification."
  (assert (listp attributes))
  (let* ((attributesstring
	 (concatenate 'string
	  (if attributes
	      (attributes-to-xml attributes)
	      "")
	  (if verbatim-attributes (concatenate 'string " " verbatim-attributes) "")))
	(return-string
	 (if (noes namespace)
	     (concatenate 'string "<" label attributesstring ">")
	     (concatenate 'string "<" namespace ":" label attributesstring ">"))))
    (if stream (write-string return-string stream) return-string)))

(defun prologue (&optional s)
  (write-string "<?xml version=\"1.0\"?>" s))

(defun xml-spec (&key stylesheets stream)
  "Return a string corresponding to the top of a XML document. This includes the prologue (<?xml ...> statement and, possibly, a stylesheet specification <?xml-stylesheet ...>) and the DOCTYPE document type declaration. If STYLESHEETS is non-nil, then <?xml-stylesheet ...> declarations are specified with STYLESHEETS (see XML-STYLESHEET for format of STYLESHEETS). Note that it may not be desirable to specify stylesheets here; in a HTML document, the stylesheet can also be specified using the 'STYLE' tag in the HEAD component of the document."
  (assert (if stylesheets (listp (first stylesheets)) t))
  (prologue stream)
  (write-string
   (with-output-to-string (s)
     (if stylesheets
	 (write-string (xml-stylesheet stylesheets) s))
     (format s "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))
   stream))

(defun xml-stylesheet (stylesheets)
  "STYLESHEETS is a list where each member is a list of the form (href type). Return string representing one or more <?xml-stylesheet ...?> clauses."
  (with-output-to-string (s)
    (dolist (stylesheet-spec stylesheets)
      (let ((href (first stylesheet-spec))
	    (type (second stylesheet-spec)))
	(format s "<?xml-stylesheet href=\"~A\" type=\"~A\"?>"
		href type)))))

(defun xmlc (name some-string &key attr namespace stream)
  "Return as a string a <name>...</name> component of a XML document where SOME-STRING (a string or NIL) is included verbatim as the value of the node. If SOME-STRING is NIL, return <tag ... />. ATTR is an alist of strings where, for each member, the car is the string corresponding to the attribute name and the cadr is the attribute value. If STREAM is non-NIL, write string to stream STREAM. NAMESPACE is a string corresponding to the namespace."
  (declare (string name)
	   (list attr))
  ;(assert (noas some-string) nil (format nil "SOME-STRING should be a string or nil; instead it was ~A." some-string))
  (let ((return-string
	 (if some-string
	     (concatenate 'string
	      (start-tag name :attributes attr :namespace namespace)
	      some-string 
	      (end-tag name :namespace namespace))
	     (empty-tag name :namespace namespace :attributes attr))))
    (if stream (write-string return-string stream) return-string)))

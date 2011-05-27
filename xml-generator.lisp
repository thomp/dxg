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



;; FIXME: merge this functionality with XMLC (xmlc supports namespaces)
;; (exml "a" "")
(defun exml (label string-content &key attributes stream)
  "Return <label>string-content</label>. ATTRIBUTES is NIL or a list of lists of the form ((attribute1 value1) (attribute2 value2) ...). STRING-CONTENT is a lisp object which, if not a string, will be converted to one (hopefully...)."
  (error "Use XMLC and convert second arg to string in advance.")
  #+DAT-DEV (dat-dev:dprinter 5 "EXML.00: label" label "  string-content" string-content "  attributes" attributes "  stream" stream)
  (emit-xml label 
	    :children-string (coerce-to-string string-content)
	    :stream stream
	    :attributes attributes))

 ;; test:
  ;; TTEST-LISP> (dat-xml::emit-xml "data")
  ;; "<data />"
(defun emit-xml (label &key empty-tags-p namespace children-string attributes stream)
  "Output to stream STREAM a string representing an XML element with local name LABEL qualified with NAMESPACE, with text content CHILDREN-STRING, and with attributes ATTRIBUTES. ATTRIBUTES is NIL or a list of lists of the form ((attribute1 value1) (attribute2 value2) ...). If CHILDREN-STRING is non-nil, it should be a string. Such a string is included verbatim between the xml start and end tags specified by LABEL. 
Note(s):
- prologue, XML declarations, and/or document type declarations should be generated elsewhere"
  (declare (string label))
  (error "Use XMLC and provide CHILDREN-STRING as second argument.")
  #+DAT-DEV (dat-dev:dprinter 5 "EMIT-XML.00: label" label "  attributes" attributes "  empty-tags-p" empty-tags-p)
  ;; sanity checks
  (assert (if children-string (stringp children-string) t))
  (format stream
	  (if (or (not (noes children-string)) 
		  (not empty-tags-p))
	      (with-output-to-string (s)
		(start-tag label :namespace namespace :attributes attributes :stream s)
		(if children-string (write-string children-string s))
		(end-tag label :namespace namespace :stream s))
	      (empty-tag label :namespace namespace :attributes attributes)
	      )))



  ;; test: TTEST-LISP> (dat-xml::empty-tag "joejoe")
  ;; "<joejoe />"

;; TTEST> (dat-xml::empty-tag "a")
;; "<a  />"
(defun empty-tag (label &key namespace attributes (attributes-string "") stream)
  "Return a string. ATTRIBUTES is a list of lists of the form ((attribute1 value1) (attribute2 value2) ...). ATTRIBUTES-STRING is a string placed at position where attributes are specified."
  #+DAT-DEV (dat-dev:dprinter 6 "START-TAG.00: label" label "  attributes" attributes)
  (assert (listp attributes))
  (let ((attributesstring
	 (if attributes 
	     (concs (attributes-to-xml attributes) attributes-string)
	     attributes-string)))
    ;; ATTRIBUTESSTRING isn't necessarily preceded by a space
    (if (noes namespace)
	(format stream "<~A ~A />" label attributesstring)
	(format stream "<~A:~A ~A />" namespace label attributesstring))))

(defun end-tag (label &key namespace stream) ; formerly &optional namespace
  " "
  (let ((string
	 (if (noes namespace)
	     ;; format dangerous w/hunchentoot 1.0?
	     ;; OLD: (format stream "</~A>" label)
	     (concs "</" label ">") 
	     ;; OLD: (format stream "</~A:~A>" namespace label)
	     (concs "</" namespace ":" label ">")
	     )))
    (if stream (write-string string stream) string)))

;; test: spacing nice when attributes and verbatim-attributes present
;; (start-tag "yo" :attributes (list (list "a" "aval") (list "b" "bval")) :verbatim-attributes "a=b")
;; test: spacing nice w/only verbatim attributes
;; TTEST> (start-tag "yo" :verbatim-attributes "a=b")
;; "<yo a=b>"
;; test: spacing nice w/only attributes
;; TTEST> (start-tag "yo" :attributes (list (list "a" "aval") (list "b" "bval")))
;; "<yo a=\"aval\" b=\"bval\">"

(defun start-tag (label &key namespace attributes verbatim-attributes stream) ; formerly &optional namespace
  "ATTRIBUTES is a list of lists of the form ((attribute1 value1) (attribute2 value2) ...) 

If non-NIL, VERBATIM-ATTRIBUTES is a string which should be included verbatim at the attributes position of the tag.

VALUEn can be ?? string? any object? If VALUEn is NIL, ignore that attribute specification."
  #+DAT-DEV (dat-dev:dprinter 6 "START-TAG.00: label")
  #+DAT-DEV (dat-dev:dprinter 6 "  attributes" attributes)
  (assert (listp attributes))
  (let* ((attributesstring
	 (concs
	  (if attributes
	      (attributes-to-xml attributes)
	      "")
	  (if verbatim-attributes (concs " " verbatim-attributes) "")))
	(return-string
	 (if (noes namespace)
	     (concs "<" label attributesstring ">")
	     (concatenate 'string "<" namespace ":" label attributesstring ">"))))
    (if stream (write-string return-string stream) return-string)))

;; FIXME: merge functionality with xml-utils.lisp/EXML (which doesn't support namespaces)
;; test:
;; OTL> (dat-xml::xmlc "foo" "glurp" :attributes '(("a" . "b") ("c" . "d")) :namespace "XX")
;; "<XX:foo a=\"b\" c=\"d\">glurp</XX:foo>"

;; TREPL> (dxg:xmlc "foo" "goo" :attr '(("a" "b")))
;; "<foo a=\"b\">goo</foo>"
(defun xmlc (name some-string &key attr namespace stream)
  "Return as a string a <name>...</name> component of a XML document where SOME-STRING (a string or NIL) is included verbatim as the value of the node. If SOME-STRING is NIL, return <tag ... />. ATTR is an alist of strings where, for each member, the car is the string corresponding to the attribute name and the cadr is the attribute value. If STREAM is non-NIL, write string to stream STREAM. NAMESPACE is a string corresponding to the namespace."
  (declare (string name))
  ;(assert (stringp tag))
  (assert (listp attr))
  ;(assert (dat-cl-utils:noas some-string) nil (format nil "SOME-STRING should be a string or nil; instead it was ~A." some-string))
  (let ((return-string
	 (if some-string
	     (concs
	      (start-tag name :attributes attr :namespace namespace)
	      some-string 
	      (end-tag name :namespace namespace))
	     (empty-tag name :namespace namespace :attributes attr))))
    (if stream (write-string return-string stream) return-string)))
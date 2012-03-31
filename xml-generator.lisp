(in-package :dxg)
;;;
;;; xml-generator.lisp: generate XML strings
;;;

;; 06b
(defun attributes-to-xml (attributes stream)
  "Given a list of lists where each sublist has the form (attribute1 value1), write equivalent of string representing the attributes in the form used in an XML document to stream STREAM."
  (block outer
    (tagbody 
     top
       (let ((apair (pop attributes))) 
	 (if apair
	     (let ((v (second apair)))
	       (if v 	    
		   (format stream " ~A=\"~A\"" (first apair) v )))))
       (if (null attributes) 
	   (return-from outer) 
	   (go top)))))

(defun empty-tag (label &key namespace attributes (attributes-string "") stream)
  "Return a string. ATTRIBUTES is a list of lists of the form ((attribute1 value1) (attribute2 value2) ...). ATTRIBUTES-STRING is a string placed at position where attributes are specified."
  (assert (listp attributes))
  (write-string
   (with-output-to-string (s)
     (empty-tag* label :namespace namespace
		 :attributes attributes
		 :attributes-string attributes-string
		 :stream s))
   stream))

(defun empty-tag* (label &key namespace attributes (attributes-string "") stream)
  (write-char #\< stream)
  (when (not (noes namespace))
    (write-string namespace stream)
    (write-char #\: stream))
  (write-string label stream) 
  ;; ATTRIBUTES-TO-XML introduces a space preceding attributes
  (attributes-to-xml attributes stream)
  (when attributes-string
    (write-char #\Space stream)
    (write-string attributes-string stream))
  (write-char #\/ stream)
  (write-char #\> stream))

(defun end-tag (label &key namespace stream)
  (let ((string
	 (with-output-to-string (s)
	   (end-tag* label :namespace namespace :stream s))))
    (if stream (write-string string stream) string)))

(defun end-tag* (label &key namespace stream)
  (write-string "</" stream)
  (unless (noes namespace)
    (write-string namespace stream)
    (write-char #\: stream))
  (write-string label stream)
  (write-char #\> stream))

(defun start-tag (label &key namespace attributes verbatim-attributes stream) ; formerly &optional namespace
  "ATTRIBUTES is a list of lists of the form ((attribute1 value1) (attribute2 value2) ...). If non-NIL, VERBATIM-ATTRIBUTES is a string which should be included verbatim at the attributes position of the tag."
  (declare (list attributes))
  (let ((return-string
	 (with-output-to-string (s)
	   (start-tag* label :namespace namespace
		       :attributes attributes
		       :verbatim-attributes verbatim-attributes
		       :stream s))))
    (if stream
	(write-string return-string stream) 
	return-string)))

(defun start-tag* (label &key namespace attributes verbatim-attributes stream) 
  (write-char #\< stream)
  (unless (noes namespace)
    (progn
      (write-string namespace stream)
      (write-char #\: stream)))
  (write-string label stream)
  (if attributes
      (attributes-to-xml attributes stream))
  (if verbatim-attributes 
      (progn
	(write-char #\Space stream)
	(write-string verbatim-attributes stream))) 
  (write-char #\> stream))

(defun prologue (&optional s)
  (write-string "<?xml version=\"1.0\"?>" s))

(defun xml-spec (&key stylesheets stream)
  "Return a string corresponding to the top of a XML document. This includes the prologue (<?xml ...> statement and, possibly, a stylesheet specification <?xml-stylesheet ...>) and the DOCTYPE document type declaration. If STYLESHEETS is non-nil, then <?xml-stylesheet ...> declarations are specified with STYLESHEETS (see XML-STYLESHEET for format of STYLESHEETS). Note that it may not be desirable to specify stylesheets here; in a HTML document, the stylesheet can also be specified using the 'STYLE' tag in the HEAD component of the document."
  ;;(assert (if stylesheets (listp (first stylesheets)) t))
  (prologue stream)
  (write-string
   (with-output-to-string (s)
     (if stylesheets
	 (write-string (xml-stylesheet stylesheets) s))
     (write-string "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" s))
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
  "Return, as a string, a <name>...</name> component of a XML document. SOME-STRING, if a string, is included verbatim as the value of the node. If SOME-STRING is NIL, <name ... /> is returned. ATTR is an alist of strings where, for each member, the car is the string corresponding to the attribute name and the cadr is the attribute value. If STREAM is non-NIL, write string to stream STREAM. NAMESPACE is a string corresponding to the namespace."
  (declare (string name)
	   (list attr))
  (let ((return-string
	 (with-output-to-string (s) (xmlc* name some-string :attr attr :namespace namespace :stream s))))
    (if stream (write-string return-string stream) return-string)))

(defun xmlc* (name some-string &key attr namespace stream)
  (declare (string name)
	   (list attr))
  (if some-string
      (progn
	(start-tag* name :attributes attr :namespace namespace :stream stream)
	(write-string some-string stream)
	(end-tag* name :namespace namespace :stream stream))
      (empty-tag* name :namespace namespace :attributes attr :stream stream)))

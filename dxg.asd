;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DXG; Base: 10 -*-
;;;
;;; dxg.asd: the dxg system definition
;;;
(defsystem dxg
    :serial t
    :components ((:file "packages")
		 (:file "util")
		 (:file "xml-generator")))
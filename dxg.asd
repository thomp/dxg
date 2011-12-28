;;;
;;; dxg.asd: the dxg system definition
;;;
(defsystem dxg
    :serial t
    :components ((:file "packages") 
		 (:file "util")
		 (:file "named-entities")
		 (:file "xml-generator")))
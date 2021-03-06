(in-package :cl-user)

(defpackage :dxg
  (:use :cl) 
  (:documentation "The program is released under the terms of the Lisp Lesser GNU Public License http://opensource.franz.com/preamble.html, also known as the LLGPL. Copyright: David A. Thompson, 2006-2016")
  (:export
   char-to-xml-entity
   el<
   elt+
   empty-tag
   end-tag
   start-tag
   xml-spec
   xmlc
   xmlc*))

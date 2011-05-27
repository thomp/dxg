;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
(in-package :cl-user)

(defpackage :dxg			; DAT XML generator
  (:use :cl) 
  (:documentation "The program is released under the terms of the Lisp Lesser GNU Public License http://opensource.franz.com/preamble.html, also known as the LLGPL. Copyright: David A. Thompson, 2006-2011")
  (:export
   empty-tag
   end-tag
   start-tag
   xmlc))
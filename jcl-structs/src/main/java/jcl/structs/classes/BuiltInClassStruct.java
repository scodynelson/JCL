package jcl.structs.classes;

public class BuiltInClassStruct extends ClassStruct {

	/*
	;;; BUILT-IN-CLASS is used to represent the standard classes that aren't
;;; defined with DEFSTRUCT and other specially implemented primitve types whose
;;; only attribute is their name.
;;;
;;; Some BUILT-IN-CLASSes have a TRANSLATION, which means that they are
;;; effectively DEFTYPE'd to some other type (usually a union of other classes
;;; or a "primitive" type such as NUMBER, ARRAY, etc.)  This translation is
;;; done when type specifiers are parsed.  Type system operations (union,
;;; subtypep, etc.) should never encounter translated classes, only their
;;; translation.
;;;
(defstruct (built-in-class (:include class))
  ;;
  ;; Type we translate to on parsing.  If NIL, then this class stands on its
  ;; own.  Only :INITIALIZING for a period during cold-load.  See below.
  (translation nil :type (or ctype (member nil :initializing))))
	 */
}

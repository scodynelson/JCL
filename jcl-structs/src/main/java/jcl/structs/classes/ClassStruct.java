package jcl.structs.classes;

public class ClassStruct extends StandardObjectStruct {

	/*
	(defstruct (class
	     (:conc-name %class-)
	     (:make-load-form-fun class-make-load-form-fun)
	     (:print-function %print-class)
	     (:include ctype
		       (:class-info (type-class-or-lose 'class)))
	     (:pure nil))
  ;;
  ;; Optional name, for printing.
  (name nil)
  ;;
  ;; Current layout for this class.  Null if not assigned yet.
  (layout nil :type (or layout null))
  ;;
  ;; How sure we are that this class won't be redefined.  If :READ-ONLY, we are
  ;; committed to not changing the effective slots or superclasses.  If
  ;; :SEALED, we can't even add subclasses.
  (state nil :type (member nil :read-only :sealed))
  ;;
  ;; Direct superclasses of this class.
  (direct-superclasses () :type list)
  ;;
  ;; Representation of all of the subclasses (direct or indirect) of this
  ;; class.  NIL if no subclasses or not initalized yet.  Otherwise, an EQ
  ;; hash-table mapping class-objects to the subclass layout that was in effect
  ;; at the time the subclass was created.
  (subclasses nil :type (or hash-table null))
  ;;
  ;; The PCL class object, or NIL if none assigned yet.
  (pcl-class nil))
	 */
}

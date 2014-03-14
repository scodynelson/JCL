package jcl.structs;

import jcl.LispStruct;
import jcl.LispType;

public class ConditionStruct implements LispStruct {

	/*
(defstruct (condition-class (:include slot-class))
  ;;
  ;; List of CONDITION-SLOT structures for the direct slots of this class.
  (slots nil :type list)
  ;;
  ;; List of CONDITION-SLOT structures for all of the effective class slots of
  ;; this class.
  (class-slots nil :type list)
  ;;
  ;; Report function or NIL.
  (report nil :type (or function null))
  ;;
  ;; List of alternating initargs and initforms.
  (default-initargs () :type list)
  ;;
  ;; CPL as a list of class objects, with all non-condition classes removed.
  (cpl () :type list)
  ;;
  ;; A list of all the effective instance allocation slots of this class that
  ;; have a non-constant initform or default-initarg.  Values for these slots
  ;; must be computed in the dynamic environment of MAKE-CONDITION.
  (hairy-slots nil :type list))

(defstruct (condition
	    (:constructor make-condition-object (actual-initargs))
	    (:alternate-metaclass instance condition-class
				  make-condition-class))

  (function-name nil)
  ;;
  ;; Actual initargs supplied to MAKE-CONDITION.
  (actual-initargs (required-argument) :type list)
  ;;
  ;; Plist mapping slot names to any values that were assigned or defaulted
  ;; after creation.
  (assigned-slots () :type list))
	 */

	@Override
	public LispType getType() {
		return null;
	}
}

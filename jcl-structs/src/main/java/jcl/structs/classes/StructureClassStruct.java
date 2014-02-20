package jcl.structs.classes;

import jcl.types.LispType;
import jcl.types.classes.StructureClass;

public class StructureClassStruct extends ClassStruct {

	/*
	;;; Class with print function, but not necessarily a structure class.
;;; (CONDITIONs)
;;;
(defstruct (slot-class (:include class))
  ;;
  ;; Print function, or NIL if none.
  (print-function nil :type (or function symbol null)))

;;; STRUCTURE-CLASS represents what we need to know about structure classes.
;;; Non-structure "typed" defstructs are a special case, and don't have a
;;; corresponding class.
;;;
(defstruct (basic-structure-class (:include slot-class)))

(defstruct (structure-class (:include basic-structure-class))
  ;;
  ;; MAKE-LOAD-FORM method, or NIL if none. :J-D-I-N dumps the slots.
  ;; :IGNORE-IT is used for magic structures which the compiler inserts in IR1,
  ;; but that are never actually dumped.
  (make-load-form-fun nil :type (or function symbol
				    (member :just-dump-it-normally
					    :ignore-it
					    nil)))
	 */

	@Override
	public LispType getType() {
		return StructureClass.INSTANCE;
	}
}

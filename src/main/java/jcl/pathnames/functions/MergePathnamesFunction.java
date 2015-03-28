/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import jcl.LispStruct;
import jcl.functions.FunctionStruct;
import jcl.pathnames.PathnameStruct;
import org.springframework.stereotype.Component;

@Component
public class MergePathnamesFunction extends FunctionStruct {

	private static final long serialVersionUID = 3634903325863235363L;

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		// TODO
		return null;
	}

	public PathnameStruct mergePathnames(final LispStruct arg1, final LispStruct arg2) {
		// TODO
		return (PathnameStruct) arg1;
	}

/*

;;; MERGE-PATHNAMES -- Interface
;;;
(defun merge-pathnames (pathname &optional (defaults *default-pathname-defaults*)
										   (default-version :newest))
  (with-pathname (defaults defaults)
    (let ((pathname (let ((*default-pathname-defaults* defaults))
		              (pathname pathname))))
      (let* ((default-host (%pathname-host defaults))
		     (pathname-host (%pathname-host pathname))
		     (diddle-case (and default-host pathname-host
							   (not (eq (host-customary-case default-host)
								    (host-customary-case pathname-host))))))
		(make-pathname :host (or pathname-host default-host)
					   :device (or (%pathname-device pathname)
						           (maybe-diddle-case (%pathname-device defaults) diddle-case))
					   :directory (merge-directories (%pathname-directory pathname)
									                 (%pathname-directory defaults)
									                 diddle-case)
					   :name (or (%pathname-name pathname)
							     (maybe-diddle-case (%pathname-name defaults)
									                diddle-case))
					   :type (or (%pathname-type pathname)
							     (maybe-diddle-case (%pathname-type defaults)
									                diddle-case))
					   :version (or (if (null (%pathname-name pathname))
										(or (%pathname-version pathname)
										    (%pathname-version defaults))
								      (%pathname-version pathname))
							        default-version))))))


;;; MERGE-DIRECTORIES -- Internal
;;;
(defun merge-directories (dir1 dir2 diddle-case)
  (if (or (eq (car dir1) :absolute)
	      (null dir2))
      dir1
    (let ((results nil))
	  (flet ((add (dir)
			   (if (and (eq dir :back)
				        (cdr results)
				        (not (eq (car results) :back)))
			       (pop results)
			     (push dir results))))
	    (dolist (dir (maybe-diddle-case dir2 diddle-case))
	      (add dir))
	    (dolist (dir (cdr dir1))
	      (add dir)))
	  (reverse results))))


;;; MAYBE-DIDDLE-CASE  -- Internal
;;;
;;;   Change the case of thing if diddle-p T.
;;;
(defun maybe-diddle-case (thing diddle-p)
  (if (and diddle-p (not (or (symbolp thing) (integerp thing))))
      (labels ((check-for (pred in)
				 (typecase in
				   (pattern
				    (dolist (piece (pattern-pieces in))
				      (when (typecase piece
					          (simple-string
					           (check-for pred piece))
					          (cons
					           (case (car in)
						        (:character-set (check-for pred (cdr in))))))
						(return t))))
				   (list
				    (dolist (x in)
				      (when (check-for pred x)
						(return t))))
				   (simple-base-string
				    (dotimes (i (length in))
				      (when (funcall pred (schar in i))
						(return t))))
				   (t
				    nil)))
	           (diddle-with (fun thing)
			     (typecase thing
			       (pattern
			        (make-pattern (mapcar #'(lambda (piece)
											  (typecase piece
											    (simple-base-string
											     (funcall fun piece))
											    (cons
											     (case (car piece)
											      (:character-set
											       (cons :character-set (funcall fun (cdr piece))))
											      (t
											       piece)))
											     (t
											      piece)))
										  (pattern-pieces thing))))
				   (list
				    (mapcar fun thing))
				   (simple-base-string
				    (funcall fun thing))
				   (t
				    thing))))
		(let ((any-uppers (check-for #'upper-case-p thing))
		      (any-lowers (check-for #'lower-case-p thing)))
		  (cond ((and any-uppers any-lowers)
			     ;; Mixed case, stays the same.
			     thing)
				(any-uppers
				 ;; All uppercase, becomes all lower case.
				 (diddle-with #'(lambda (x)
				                  (if (stringp x)
									  (string-downcase x)
								     x))
						      thing))
				(any-lowers
				 ;; All lowercase, becomes all upper case.
				 (diddle-with #'(lambda (x)
				                  (if (stringp x)
									  (string-upcase x)
									x))
							  thing))
				(t
				 ;; No letters?  I guess just leave it.
				 thing))))
      thing))

*/
}

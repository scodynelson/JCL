;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ACCESSOR FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun caar (list)
  "Returns the first object of the car of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Caar"))
  (car (car list)))

(defun cadr (list)
  "Returns the first object of the cdr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cadr"))
  (car (cdr list)))

(defun cdar (list)
  "Returns the cdr of the first object in a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cdar"))
  (cdr (car list)))

(defun cddr (list)
  "Returns the cdr of all but the fist item in a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cddr"))
  (cdr (cdr list)))

(defun caaar (list)
  "Returns the first object in the caar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Caaar"))
  (car (car (car list))))

(defun caadr (list)
  "Returns the first object in the cadr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Caadr"))
  (car (car (cdr list))))

(defun cadar (list)
  "Returns the first object of the cdar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cadar"))
  (car (cdr (car list))))

(defun cdaar (list)
  "Returns the cdr of the caar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cdaar"))
  (cdr (car (car list))))

(defun caddr (list)
  "Returns the first object in the cddr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Caddr"))
  (car (cdr (cdr list))))

(defun cdadr (list)
  "Returns the cdr of the cadr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cdadr"))
  (cdr (car (cdr list))))

(defun cddar (list)
  "Returns the cdr of the cdar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cddar"))
  (cdr (cdr (car list))))

(defun cdddr (list)
  "Returns the cdr of the cddr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cdddr"))
  (cdr (cdr (cdr list))))

(defun caaaar (list)
  "Returns the first object of the caaar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Caaaar"))
  (car (car (car (car list)))))

(defun caaadr (list)
  "Returns the first object of the caadr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Caaadr"))
  (car (car (car (cdr list)))))

(defun caadar (list)
  "Returns the first object of the cadar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Caadar"))
  (car (car (cdr (car list)))))

(defun caaddr (list)
  "Returns the first object of the caddr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Caaddr"))
  (car (car (cdr (cdr list)))))

(defun cadaar (list)
  "Returns the first object of the cdaar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cadaar"))
  (car (cdr (car (car list)))))

(defun cadadr (list)
  "Returns the first object of the cdadr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cadadr"))
  (car (cdr (car (cdr list)))))

(defun caddar (list)
  "Returns the first object of the cddar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Caddar"))
  (car (cdr (cdr (car list)))))

(defun cadddr (list)
  "Returns the first object of the cdddr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cadddr"))
  (car (cdr (cdr (cdr list)))))

(defun cdaaar (list)
  "Returns the cdr of the caaar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cdaaar"))
  (cdr (car (car (car list)))))

(defun cdaadr (list)
  "Returns the cdr of the caadr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cdaadr"))
  (cdr (car (car (cdr list)))))

(defun cdadar (list)
  "Returns the cdr of the cadar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cdadar"))
  (cdr (car (cdr (car list)))))

(defun cdaddr (list)
  "Returns the cdr of the caddr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cdaddr"))
  (cdr (car (cdr (cdr list)))))

(defun cddaar (list)
  "Returns the cdr of the cdaar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cddaar"))
  (cdr (cdr (car (car list)))))

(defun cddadr (list)
  "Returns the cdr of the cdadr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cddadr"))
  (cdr (cdr (car (cdr list)))))

(defun cdddar (list)
  "Returns the cdr of the cddar of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cdddar"))
  (cdr (cdr (cdr (car list)))))

(defun cddddr (list)
  "Returns the cdr of the cdddr of a list."
  (declare (system::%java-class-name "jcl.lists.functions.Cddddr"))
  (cdr (cdr (cdr (cdr list)))))

(defun first (list)
  "Returns the 1st object in a list or NIL if the list is empty."
  (declare (system::%java-class-name "jcl.lists.functions.First"))
  (car list))

(defun second (list)
  "Returns the 2nd object in a list or NIL if there is not 2nd object."
  (declare (system::%java-class-name "jcl.lists.functions.Second"))
  (car (cdr list)))

(defun third (list)
  "Returns the 3rd object in a list or NIL if there is not 3rd object."
  (declare (system::%java-class-name "jcl.lists.functions.Third"))
  (car (cddr list)))

(defun fourth (list)
  "Returns the 4th object in a list or NIL if there is not 4th object."
  (declare (system::%java-class-name "jcl.lists.functions.Fourth"))
  (car (cdddr list)))

(defun fifth (list)
  "Returns the 5th object in a list or NIL if there is not 5th object."
  (declare (system::%java-class-name "jcl.lists.functions.Fifth"))
  (car (cddddr list)))

(defun sixth (list)
  "Returns the 6th object in a list or NIL if there is not 6th object."
  (declare (system::%java-class-name "jcl.lists.functions.Sixth"))
  (car (cdr (cddddr list))))

(defun seventh(list)
  "Returns the 7th object in a list or NIL if there is not 7th object."
  (declare (system::%java-class-name "jcl.lists.functions.Seventh"))
  (car (cddr (cddddr list))))

(defun eighth (list)
  "Returns the 8th object in a list or NIL if there is not 8th object."
  (declare (system::%java-class-name "jcl.lists.functions.Eighth"))
  (car (cdddr (cddddr list))))

(defun ninth (list)
  "Returns the 9th object in a list or NIL if there is not 9th object."
  (declare (system::%java-class-name "jcl.lists.functions.Ninth"))
  (car (cddddr (cddddr list))))

(defun tenth (list)
  "Returns the 10th object in a list or NIL if there is not 10th object."
  (declare (system::%java-class-name "jcl.lists.functions.Tenth"))
  (car (cdr (cddddr (cddddr list)))))

(defun rest (list)
  "Returns all but the first object in the list."
  (declare (system::%java-class-name "jcl.lists.functions.Rest"))
  (cdr list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
     ,element))

(defmacro satisfies-the-test (item elt)
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key ,elt)))
      (cond (test-p (funcall test ,item ,key-tmp))
	        (test-not-p (not (funcall test-not ,item ,key-tmp)))
	        (t (funcall test ,item ,key-tmp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TREE-EQUAL FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tree-equal-test-not (tree-1 tree-2 test-not)
  (cond ((consp tree-1)
	     (and (consp tree-2)
	          (tree-equal-test-not (car tree-1) (car tree-2) test-not)
	          (tree-equal-test-not (cdr tree-1) (cdr tree-2) test-not)))
	    ((consp tree-2) nil)
	    ((not (funcall test-not tree-1 tree-2)) t)
	    (t nil)))

(defun tree-equal-test (tree-1 tree-2 test)
  (cond	((consp tree-1)
	     (and (consp tree-2)
	          (tree-equal-test (car tree-1) (car tree-2) test)
	          (tree-equal-test (cdr tree-1) (cdr tree-2) test)))
	    ((consp tree-2) nil)
	    ((funcall test tree-1 tree-2) t)
	    (t nil)))

;;; TREE-EQUAL tests whether two trees are of the same shape and have the same leaves. It returns true if tree-1 and tree-2
;;; are both atoms and satisfy the test, or if they are both conses and the car of tree-1 is tree-equal to the car of tree-2
;;; and the cdr of tree-1 is tree-equal to the cdr of tree-2. Otherwise, it returns false. It recursively compares conses
;;; but not any other objects that have components.
;;; Parameters:
;;;   tree-1 => a tree.
;;;   tree-2 => a tree.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;; Returns:
;;;   generalized-boolean => a generalized boolean.
;;;
(defun tree-equal (tree-1 tree-2 &key (test #'eql test-p) (test-not nil test-not-p))
  "Tests whether two trees are of the same shape and have the same leaves. It returns true if tree-1 and tree-2
   are both atoms and satisfy the test, or if they are both conses and the car of tree-1 is tree-equal to the car of tree-2
   and the cdr of tree-1 is tree-equal to the cdr of tree-2. Otherwise, it returns false."
  (declare (system::%java-class-name "jcl.lists.functions.TreeEqual"))
  (if test-not-p
      (tree-equal-test-not tree-1 tree-2 test-not)
    (tree-equal-test tree-1 tree-2 test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MEMBER FAMILY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defun member (item list &key key (test #'eql test-p) (test-not nil test-not-p))
  (do ((list list (cdr list)))
      ((null list))
    (let ((car (car list)))
      (when (satisfies-the-test item car)
	    (return list)))))
|#
;;; MEMBER searches list for item or for a top-level element that satisfies the test. The argument
;;; to the test/test-not function is an element of list. If some element satisfies the test, the tail
;;; of list beginning with this element is returned; otherwise nil is returned.
;;; Parameters:
;;;   item => an object.
;;;   list => a proper list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   tail => a list.
;;;
(defun member (item list &key key (test #'eql test-p) (test-not nil test-not-p))
  "Searches list for item or for a top-level element that satisfies the test. The argument
   to the test/test-not function is an element of list. If some element satisfies the test, the tail
   of list beginning with this element is returned; otherwise nil is returned."
  (declare (system::%java-class-name "jcl.lists.functions.Member"))
  (labels ((member-aux (list)
             (if (null list)
                 nil
               (if (satisfies-the-test item (car list))
                   list
                 (member-aux (cdr list))))))
    (member-aux list)))
#|
(defun member-if (predicate list &key key)
  (do ((list list (cdr list)))
      ((null list))
    (when (funcall predicate (apply-key key (car list)))
	  (return list))))
|#
;;; MEMBER-IF searches list for item or for a top-level element that satisfies the test. The argument
;;; to the predicate function is an element of list. If some element satisfies the test, the tail
;;; of list beginning with this element is returned; otherwise nil is returned.
;;; Parameters:
;;;   predicate => a designator for a function of one argument that returns a generalized boolean.
;;;   list => a proper list.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   tail => a list.
;;;
(defun member-if (predicate list &key key)
  "Searches list for item or for a top-level element that satisfies the test. The argument
   to the predicate function is an element of list. If some element satisfies the test, the tail
   of list beginning with this element is returned; otherwise nil is returned."
  (declare (system::%java-class-name "jcl.lists.functions.MemberIf"))
  (labels ((member-if-aux (list)
             (if (null list)
                 nil
               (if (funcall predicate (apply-key key (car list)))
                   list
                 (member-if-aux (cdr list))))))
    (member-if-aux list)))
#|
(defun member-if-not (predicate list &key key)
  (do ((list list (cdr list)))
      ((null list))
    (unless (funcall predicate (apply-key key (car list)))
	  (return list))))
|#
;;; MEMBER-IF-NOT searches list for item or for a top-level element that satisfies the test. The argument
;;; to the predicate function is an element of list. If some element satisfies the test, the tail
;;; of list beginning with this element is returned; otherwise nil is returned.
;;; Parameters:
;;;   predicate => a designator for a function of one argument that returns a generalized boolean.
;;;   list => a proper list.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   tail => a list.
;;;
(defun member-if-not (predicate list &key key)
  "Searches list for item or for a top-level element that satisfies the test. The argument
   to the predicate function is an element of list. If some element satisfies the test, the tail
   of list beginning with this element is returned; otherwise nil is returned."
  (declare (system::%java-class-name "jcl.lists.functions.MemberIfNot"))
  (labels ((member-if-not-aux (list)
             (if (null list)
                 nil
               (if (not (funcall predicate (apply-key key (car list))))
                   list
                 (member-if-not-aux (cdr list))))))
    (member-if-not-aux list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ADJOIN FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ADJOIN tests whether item is the same as an existing element of list. If the item is not an existing element,
;;; it is then added to list (as if by cons) and returned as the resulting list; otherwise, nothing is added and
;;; the original list is returned.
;;; Parameters:
;;;   item => an object.
;;;   list => a proper list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   new-list => a list.
;;;
(defun adjoin (item list &key key (test #'eql test-p) (test-not nil test-not-p))
  "Tests whether item is the same as an existing element of list; if so, it is added to the list and returned;
   otherwise, nothing is added and the original list is returned."
  (declare (system::%java-class-name "jcl.lists.functions.Adjoin"))
  (if (let ((key-val (apply-key key item)))
	    (if test-not-p
	        (member key-val list :test-not test-not :key key)
	      (member key-val list :test test :key key)))
      list
    (cons item list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ASSOC/RASSOC FAMILY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defmacro assoc-guts (test-guy)
  `(do ((alist alist (cdr alist)))
       ((null alist))
     (when (and (car alist) ,test-guy)
       (return (car alist)))))
|#
(defmacro assoc-guts (test-guy)
  `(labels ((assoc-guts-aux (alist)
             (if (null alist)
                 nil
               (if (and (car alist) ,test-guy)
                   (car alist)
                 (assoc-guts-aux (cdr alist))))))
    (assoc-guts-aux alist)))

;;; ASSOC returns the first cons in alist whose car satisfies the test, or nil if no such cons is found.
;;; If nil appears in alist in place of a pair, it is ignored.
;;; Parameters:
;;;   item => an object.
;;;   alist => an association list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   entry => a cons that is an element of alist, or nil.
;;;
(defun assoc (item alist &key key (test #'eql test-p) (test-not nil test-not-p))
  "Returns the first cons in alist whose car satisfies the test, or nil if no such cons is found.
   If nil appears in alist in place of a pair, it is ignored."
  (declare (system::%java-class-name "jcl.lists.functions.Assoc"))
  (cond (test-not-p
         (if key
             (assoc-guts (not (funcall test-not item (funcall key (caar alist)))))
           (assoc-guts (not (funcall test-not item (caar alist))))))
        (test-p
	     (if key
	         (assoc-guts (funcall test item (funcall key (caar alist))))
	       (assoc-guts (funcall test item (caar alist)))))
        (t
         (if key
             (assoc-guts (eql item (funcall key (caar alist))))
           (assoc-guts (eql item (caar alist)))))))

;;; ASSOC-IF returns the first cons in alist whose car satisfies the test, or nil if no such cons is found.
;;; If nil appears in alist in place of a pair, it is ignored.
;;; Parameters:
;;;   predicate => a designator for a function of one argument that returns a generalized boolean.
;;;   alist => an association list.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   entry => a cons that is an element of alist, or nil.
;;;
(defun assoc-if (predicate alist &key key)
  "Returns the first cons in alist whose car satisfies the test, or nil if no such cons is found.
   If nil appears in alist in place of a pair, it is ignored."
  (declare (system::%java-class-name "jcl.lists.functions.AssocIf"))
  (if key
      (assoc-guts (funcall predicate (funcall key (caar alist))))
    (assoc-guts (funcall predicate (caar alist)))))

;;; ASSOC-IF-NOT returns the first cons in alist whose car satisfies the test, or nil if no such cons is found.
;;; If nil appears in alist in place of a pair, it is ignored.
;;; Parameters:
;;;   predicate => a designator for a function of one argument that returns a generalized boolean.
;;;   alist => an association list.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   entry => a cons that is an element of alist, or nil.
;;;
(defun assoc-if-not (predicate alist &key key)
  "Returns the first cons in alist whose car satisfies the test, or nil if no such cons is found.
   If nil appears in alist in place of a pair, it is ignored."
  (declare (system::%java-class-name "jcl.lists.functions.AssocIfNot"))
  (if key
      (assoc-guts (not (funcall predicate (funcall key (caar alist)))))
    (assoc-guts (not (funcall predicate (caar alist))))))

;;; RASSOC returns the first cons in alist whose cdr satisfies the test, or nil if no such cons is found.
;;; If nil appears in alist in place of a pair, it is ignored.
;;; Parameters:
;;;   item => an object.
;;;   alist => an association list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   entry => a cons that is an element of alist, or nil.
;;;
(defun rassoc (item alist &key key (test #'eql test-p) (test-not nil test-not-p))
  "Returns the first cons in alist whose cdr satisfies the test, or nil if no such cons is found.
   If nil appears in alist in place of a pair, it is ignored."
  (declare (system::%java-class-name "jcl.lists.functions.Rassoc"))
  (cond (test-not-p
         (if key
             (assoc-guts (not (funcall test-not item (funcall key (cdar alist)))))
           (assoc-guts (not (funcall test-not item (cdar alist))))))
	    (test-p
    	 (if key
    	     (assoc-guts (funcall test item (funcall key (cdar alist))))
    	   (assoc-guts (funcall test item (cdar alist)))))
    	(t
         (if key
             (assoc-guts (eql item (funcall key (cdar alist))))
           (assoc-guts (eql item (cdar alist)))))))

;;; RASSOC-IF returns the first cons in alist whose cdr satisfies the test, or nil if no such cons is found.
;;; If nil appears in alist in place of a pair, it is ignored.
;;; Parameters:
;;;   predicate => a designator for a function of one argument that returns a generalized boolean.
;;;   alist => an association list.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   entry => a cons that is an element of alist, or nil.
;;;
(defun rassoc-if (predicate alist &key key)
  "Returns the first cons in alist whose cdr satisfies the test, or nil if no such cons is found.
   If nil appears in alist in place of a pair, it is ignored."
  (declare (system::%java-class-name "jcl.lists.functions.RassocIf"))
  (if key
      (assoc-guts (funcall predicate (funcall key (cdar alist))))
    (assoc-guts (funcall predicate (cdar alist)))))

;;; RASSOC-IF-NOT returns the first cons in alist whose cdr satisfies the test, or nil if no such cons is found.
;;; If nil appears in alist in place of a pair, it is ignored.
;;; Parameters:
;;;   predicate => a designator for a function of one argument that returns a generalized boolean.
;;;   alist => an association list.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   entry => a cons that is an element of alist, or nil.
;;;
(defun rassoc-if-not (predicate alist &key key)
  "Returns the first cons in alist whose cdr satisfies the test, or nil if no such cons is found.
   If nil appears in alist in place of a pair, it is ignored."
  (declare (system::%java-class-name "jcl.lists.functions.RassocIfNot"))
  (if key
      (assoc-guts (not (funcall predicate (funcall key (cdar alist)))))
    (assoc-guts (not (funcall predicate (cdar alist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAP FAMILY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defun map1 (function original-arglists accumulate take-car)
  "This function is called by MAPC, MAPCAR, MAPCAN, MAPL, MAPLIST, and MAPCON.
  It Maps function over the arglists in the appropriate way. It is done when any
  of the arglists runs out.  Until then, it CDRs down the arglists calling the
  function and accumulating results as desired."
  (let* ((arglists (copy-list original-arglists))
	     (ret-list (list nil))
	     (temp ret-list))
    (do ((res nil)
	     (args nil nil))
	    ((dolist (x arglists nil)
	       (unless x
	        (return t)))
         (if accumulate
             (cdr ret-list)
           (car original-arglists)))
      (do ((l arglists (cdr l)))
	      ((null l))
	    (push (if take-car
	              (caar l)
	            (car l))
	          args)
	    (setf (car l) (cdar l)))
      (setq res (apply function (nreverse args)))
      (case accumulate
        (:nconc (setq temp (last (nconc temp res))))
        (:list (rplacd temp (list res))
	           (setq temp (cdr temp)))))))

(defun mapx-aux-helper (function original-arglists accumulate take-car)
  "This function is called by MAPC, MAPCAR, MAPCAN, MAPL, MAPLIST, and MAPCON.
   It Maps function over the arglists in the appropriate way. It is done when any
   of the arglists runs out.  Until then, it CDRs down the arglists calling the
   function and accumulating results as desired."
  (labels ((mapx-aux-1 (function accumulate take-car arglists ret-list temp res &optional original-arglists)
             (let ((args nil))
               (cond ((equal nil (mapx-aux-3 arglists))
                      (setq args (mapx-aux-2 arglists args take-car))
                      (setq res (apply function (nreverse args)))
                      (case accumulate
                        (:nconc (setq temp (last (nconc temp res))))
                        (:list (rplacd temp (list res))
                               (setq temp (cdr temp))))
                      (mapx-aux-1 function accumulate take-car arglists ret-list temp res)))
               (if original-arglists
                   (if accumulate
                       (cdr ret-list)
                     (car original-arglists)))))
           (mapx-aux-2 (arglists args take-car)
             (let ((l arglists))
               (cond ((not (null l))
                      (setq args (cons (if take-car
                                           (caar l)
                                         (car l))
                                       args))
                      (rplaca l (cdar l))
                      (setq args (mapx-aux-2 (cdr l) args take-car))))
               args))
           (mapx-aux-3 (arglists)
             (let ((x (car arglists)))
               (if (null x)
                   t
                 (unless (equal x (car (last arglists)))
                   (mapx-aux-3 (cdr arglists)))))))
    (let* ((arglists (copy-list original-arglists))
           (ret-list (list nil))
           (temp ret-list)
           (res nil))
      (mapx-aux-1 function accumulate take-car arglists ret-list temp res original-arglists))))

;;; MAPC operates on successive elements of the lists, ie. function is applied to the first element of each list,
;;; then to the second element of each list, and so on. The iteration terminates when the shortest list runs out,
;;; and excess elements in other lists are ignored. The results of applying function are not accumulated and the
;;; list argument is returned.
;;;
;;; The mapping operation involves applying function to successive sets of arguments in which one argument is obtained
;;; from each sequence. The resulting sequence is list. Function is called first on all the elements with index 0,
;;; then on all those with index 1, and so on. Result-type specifies the type of the resulting sequence. If function
;;; is a symbol, it is coerced to a function as if by symbol-function.
;;;
;;; Parameters:
;;;   function => a designator for a function that must take as many arguments as there are lists.
;;;   list => a proper list.
;;; Returns:
;;;   list-1 => the first list (which must be a proper list).
;;;
(defun mapc (function &rest list)
  "Operates on successive elements of the lists, ie. function is applied to the first element of each list,
   then to the second element of each list, and so on. The iteration terminates when the shortest list runs out,
   and excess elements in other lists are ignored. Fhe results of applying function are not accumulated and the
   list argument is returned."
  (declare (system::%java-class-name "jcl.lists.functions.Mapc"))
  (mapx-aux-helper function list nil t))

;;; MAPCAR operates on successive elements of the lists. Function is applied to the first element of each list,
;;; then to the second element of each list, and so on. The iteration terminates when the shortest list runs out,
;;; and excess elements in other lists are ignored. The value returned by mapcar is a list of the results of
;;; successive calls to function.
;;;
;;; The mapping operation involves applying function to successive sets of arguments in which one argument is obtained
;;; from each sequence. The resulting sequence is list. Function is called first on all the elements with index 0,
;;; then on all those with index 1, and so on. Result-type specifies the type of the resulting sequence. If function
;;; is a symbol, it is coerced to a function as if by symbol-function.
;;;
;;; Parameters:
;;;   function => a designator for a function that must take as many arguments as there are lists.
;;;   list => a proper list.
;;; Returns:
;;;   result-list => a list.
;;;
(defun mapcar (function &rest list)
  "Operates on successive elements of the lists. Function is applied to the first element of each list,
   then to the second element of each list, and so on. The iteration terminates when the shortest list runs out,
   and excess elements in other lists are ignored. The value returned by mapcar is a list of the results of
   successive calls to function."
  (declare (system::%java-class-name "jcl.lists.functions.Mapcar"))
  (mapx-aux-helper function list :list t))

;;; MAPCAN operates on successive elements of the lists, ie. function is applied to the first element of each list,
;;; then to the second element of each list, and so on. The iteration terminates when the shortest list runs out,
;;; and excess elements in other lists are ignored. The results of applying function are combined into a list by
;;; the use of nconc rather than list.
;;;
;;; The mapping operation involves applying function to successive sets of arguments in which one argument is obtained
;;; from each sequence. The resulting sequence is list. Function is called first on all the elements with index 0,
;;; then on all those with index 1, and so on. Result-type specifies the type of the resulting sequence. If function
;;; is a symbol, it is coerced to a function as if by symbol-function.
;;;
;;; Parameters:
;;;   function => a designator for a function that must take as many arguments as there are lists.
;;;   list => a proper list.
;;; Returns:
;;;   concatenated-results => a list.
;;;
(defun mapcan (function &rest list)
  "Operates on successive elements of the lists, ie. function is applied to the first element of each list,
   then to the second element of each list, and so on. The iteration terminates when the shortest list runs out,
   and excess elements in other lists are ignored. The results of applying function are combined into a list by
   the use of nconc rather than list."
  (declare (system::%java-class-name "jcl.lists.functions.Mapcan"))
  (mapx-aux-helper function list :nconc t))

;;; MAPL operates on successive elements of the lists. Function is applied to successive sublists of the lists.
;;; Function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of
;;; the cdr of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements
;;; in other lists are ignored. The results of applying function are not accumulated and the list argument is returned.
;;;
;;; The mapping operation involves applying function to successive sets of arguments in which one argument is obtained
;;; from each sequence. The resulting sequence is list. Function is called first on all the elements with index 0,
;;; then on all those with index 1, and so on. Result-type specifies the type of the resulting sequence. If function
;;; is a symbol, it is coerced to a function as if by symbol-function.
;;;
;;; Parameters:
;;;   function => a designator for a function that must take as many arguments as there are lists.
;;;   list => a proper list.
;;; Returns:
;;;   list-1 => the first list (which must be a proper list).
;;;
(defun mapl (function &rest list)
  "Operates on successive elements of the lists. Function is applied to successive sublists of the lists.
   Function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of
   the cdr of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements
   in other lists are ignored. The results of applying function are not accumulated and the list argument is returned."
  (declare (system::%java-class-name "jcl.lists.functions.Mapl"))
  (mapx-aux-helper function list nil nil))

;;; MAPLIST operates on successive elements of the lists. Function is applied to successive sublists of the lists.
;;; Function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of
;;; the cdr of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements
;;; in other lists are ignored. The value returned by maplist is a list of the results of successive calls to function.
;;;
;;; The mapping operation involves applying function to successive sets of arguments in which one argument is obtained
;;; from each sequence. The resulting sequence is list. Function is called first on all the elements with index 0,
;;; then on all those with index 1, and so on. Result-type specifies the type of the resulting sequence. If function
;;; is a symbol, it is coerced to a function as if by symbol-function.
;;;
;;; Parameters:
;;;   function => a designator for a function that must take as many arguments as there are lists.
;;;   list => a proper list.
;;; Returns:
;;;   result-list => a list.
;;;
(defun maplist (function &rest list)
  "Operates on successive elements of the lists. Function is applied to successive sublists of the lists.
   Function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of
   the cdr of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements
   in other lists are ignored. The value returned by maplist is a list of the results of successive calls to function."
  (declare (system::%java-class-name "jcl.lists.functions.Maplist"))
  (mapx-aux-helper function list :list nil))

;;; MAPCON operates on successive elements of the lists. Function is applied to successive sublists of the lists.
;;; Function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of
;;; the cdr of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements
;;; in other lists are ignored. The results of applying function are combined into a list by the use of nconc rather
;;; than list.
;;;
;;; The mapping operation involves applying function to successive sets of arguments in which one argument is obtained
;;; from each sequence. The resulting sequence is list. Function is called first on all the elements with index 0,
;;; then on all those with index 1, and so on. Result-type specifies the type of the resulting sequence. If function
;;; is a symbol, it is coerced to a function as if by symbol-function.
;;;
;;; Parameters:
;;;   function => a designator for a function that must take as many arguments as there are lists.
;;;   list => a proper list.
;;; Returns:
;;;   concatenated-results => a list.
;;;
(defun mapcon (function &rest list)
  "Operates on successive elements of the lists. Function is applied to successive sublists of the lists.
   Function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of
   the cdr of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements
   in other lists are ignored. The results of applying function are combined into a list by the use of nconc rather
   than list."
  (declare (system::%java-class-name "jcl.lists.functions.Mapcon"))
  (mapx-aux-helper function list :nconc nil))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; INTERSECTION/UNION/SET-DIFFERENCE/SET-EXCLUSIVE-OR/SUBSETP FUNCTIONS ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defmacro with-set-keys (funcall)
  `(cond (test-not-p ,(append funcall '(:key key :test-not test-not)))
	     (t ,(append funcall '(:key key :test test)))))

(defmacro steve-splice (source destination)
  `(let ((temp ,source))
    (setf ,source (cdr ,source)
	      (cdr temp) ,destination
	      ,destination temp)))

(defun intersection (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  (let ((res nil))
    (dolist (elt list1)
      (when (with-set-keys (member (apply-key key elt) list2))
	    (push elt res)))
    res))

;;; INTERSECTION returns a list that contains every element that occurs in both list-1 and list-2. For all possible
;;; ordered pairs consisting of one element from list-1 and one element from list-2, :test or :test-not are used
;;; to determine whether they satisfy the test. The first argument to the :test or :test-not function is an element
;;; of list-1; the second argument is an element of list-2. If :test or :test-not is not supplied, eql is used. If :key
;;; is supplied (and not nil), it is used to extract the part to be tested from the list element. The argument to the :key
;;; function is an element of either list-1 or list-2; the :key function typically returns part of the supplied element.
;;; If :key is not supplied or nil, the list-1 and list-2 elements are used. For every pair that satifies the test,
;;; exactly one of the two elements of the pair will be put in the result.
;;; Parameters:
;;;   list-1 => a proper list.
;;;   list-2 => a proper list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   result-list => a list.
;;;
(defun intersection (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  "Returns a list that contains every element that occurs in both list-1 and list-2."
  (declare (system::%java-class-name "jcl.lists.functions.Intersection"))
  (labels ((intersection-aux (list1 list2 res)
             (let ((elt (car list1)))
               (if (consp list1)
                   (progn (when (with-set-keys (member (apply-key key elt) list2))
                            (setq res (nconc (list elt) res)))
                          (intersection-aux (cdr list1) list2 res))
                 res))))
    (intersection-aux list1 list2 nil)))

(defun nintersection (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  (let ((res nil)
	    (list1 list1))
    (do ()
        ((null list1))
      (if (with-set-keys (member (apply-key key (car list1)) list2))
	      (steve-splice list1 res)
	    (setq list1 (cdr list1))))
    res))

;;; NINTERSECTION returns a list that contains every element that occurs in both list-1 and list-2. It may
;;; destroy list-1 using its cells to construct the result; however, list-2 is not destroyed. For all possible
;;; ordered pairs consisting of one element from list-1 and one element from list-2, :test or :test-not are used
;;; to determine whether they satisfy the test. The first argument to the :test or :test-not function is an element
;;; of list-1; the second argument is an element of list-2. If :test or :test-not is not supplied, eql is used. If :key
;;; is supplied (and not nil), it is used to extract the part to be tested from the list element. The argument to the :key
;;; function is an element of either list-1 or list-2; the :key function typically returns part of the supplied element.
;;; If :key is not supplied or nil, the list-1 and list-2 elements are used. For every pair that satifies the test,
;;; exactly one of the two elements of the pair will be put in the result.
;;; Parameters:
;;;   list-1 => a proper list.
;;;   list-2 => a proper list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   result-list => a list.
;;;
(defun nintersection (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  "Returns a list that contains every element that occurs in both list-1 and list-2."
  (declare (system::%java-class-name "jcl.lists.functions.Nintersection"))
  (labels ((nintersection-aux (list1 list2 res)
             (if (consp list1)
                 (progn (if (with-set-keys (member (apply-key key (car list1)) list2))
                            (steve-splice list1 res)
                          (setq list1 (cdr list1)))
                        (nintersection-aux list1 list2 res))
               res)))
    (nintersection-aux list1 list2 nil)))

(defun union (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  (let ((res list2))
    (dolist (elt list1)
      (unless (with-set-keys (member (apply-key key elt) list2))
	    (push elt res)))
    res))

;;; UNION returns a list that contains every element that occurs in either list-1 or list-2. For all possible
;;; ordered pairs consisting of one element from list-1 and one element from list-2, :test or :test-not are used
;;; to determine whether they satisfy the test. The first argument to the :test or :test-not function is an element
;;; of list-1; the second argument is an element of list-2. If :test or :test-not is not supplied, eql is used. If :key
;;; is supplied (and not nil), it is used to extract the part to be tested from the list element. The argument to the :key
;;; function is an element of either list-1 or list-2; the :key function typically returns part of the supplied element.
;;; If :key is not supplied or nil, the list-1 and list-2 elements are used. For every pair that satifies the test,
;;; exactly one of the two elements of the pair will be put in the result. Any element from either list-1 or list-2
;;; that matches no element of the other will appear in the result.
;;; Parameters:
;;;   list-1 => a proper list.
;;;   list-2 => a proper list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   result-list => a list.
;;;
(defun union (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  "Returns the union of list1 and list2"
  (declare (system::%java-class-name "jcl.lists.functions.Union"))
  (labels ((union-aux (list1 list2 res)
             (let ((elt (car list1)))
               (if (consp list1)
                   (progn (unless (with-set-keys (member (apply-key key elt) list2))
                            (setq res (nconc (list elt) res)))
                          (union-aux (cdr list1) list2 res))
                 res))))
    (union-aux list1 list2 list2)))

(defun nunion (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  (let ((res list2)
	    (list1 list1))
    (do ()
        ((null list1))
      (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
	      (steve-splice list1 res)
	    (setf list1 (cdr list1))))
    res))

;;; NUNION returns a list that contains every element that occurs in either list-1 or list-2. It may
;;; destroy list-1 using its cells to construct the result; however, list-2 is not destroyed. For all possible
;;; ordered pairs consisting of one element from list-1 and one element from list-2, :test or :test-not are used
;;; to determine whether they satisfy the test. The first argument to the :test or :test-not function is an element
;;; of list-1; the second argument is an element of list-2. If :test or :test-not is not supplied, eql is used. If :key
;;; is supplied (and not nil), it is used to extract the part to be tested from the list element. The argument to the :key
;;; function is an element of either list-1 or list-2; the :key function typically returns part of the supplied element.
;;; If :key is not supplied or nil, the list-1 and list-2 elements are used. For every pair that satifies the test,
;;; exactly one of the two elements of the pair will be put in the result. Any element from either list-1 or list-2
;;; that matches no element of the other will appear in the result.
;;; Parameters:
;;;   list-1 => a proper list.
;;;   list-2 => a proper list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   result-list => a list.
;;;
(defun nunion (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  "Returns the union of list1 and list2"
  (declare (system::%java-class-name "jcl.lists.functions.Nunion"))
  (labels ((nunion-aux (list1 list2 res)
             (if (consp list1)
                 (progn (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
                            (steve-splice list1 res)
                          (setq list1 (cdr list1)))
                        (nunion-aux list1 list2 res))
               res)))
    (nunion-aux list1 list2 list2)))

(defun set-difference (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  (if (null list2)
      list1
    (let ((res nil))
	  (dolist (elt list1)
	    (unless (with-set-keys (member (apply-key key elt) list2))
	      (push elt res)))
	  res)))

;;; SET-DIFFERENCE returns a list of elements of list-1 that do not appear in list-2. For all possible
;;; ordered pairs consisting of one element from list-1 and one element from list-2, the :test or :test-not
;;; function is used to determine whether they satisfy the test. The first argument to the :test or :test-not
;;; function is the part of an element of list-1 that is returned by the :key function (if supplied); the second
;;; argument is the part of an element of list-2 that is returned by the :key function (if supplied). If :key is
;;; supplied, its argument is a list-1 or list-2 element. The :key function typically returns part of the supplied
;;; element. If :key is not supplied, the list-1 or list-2 element is used. An element of list-1 appears in the result
;;; if and only if it does not match any element of list-2.
;;; Parameters:
;;;   list-1 => a proper list.
;;;   list-2 => a proper list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   result-list => a list.
;;;
(defun set-difference (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  "Returns a list of elements of list-1 that do not appear in list-2."
  (declare (system::%java-class-name "jcl.lists.functions.SetDifference"))
  (labels ((set-difference-aux (list1 list2 res)
             (let ((elt (car list1)))
               (if (consp list1)
                   (progn (unless (with-set-keys (member (apply-key key elt) list2))
                            (setq res (nconc (list elt) res)))
                          (set-difference-aux (cdr list1) list2 res))
                 res))))
    (if (null list2)
        list1
      (set-difference-aux list1 list2 nil))))

(defun nset-difference (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  (let ((res nil)
	    (list1 list1))
    (do ()
        ((null list1))
      (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
	      (steve-splice list1 res)
	    (setq list1 (cdr list1))))
    res))

;;; NSET-DIFFERENCE returns a list of elements of list-1 that do not appear in list-2. It is destructive and
;;; it may destroy list-1. For all possible ordered pairs consisting of one element from list-1 and one element
;;; from list-2, the :test or :test-not function is used to determine whether they satisfy the test. The first
;;; argument to the :test or :test-not function is the part of an element of list-1 that is returned by the :key
;;; function (if supplied); the second argument is the part of an element of list-2 that is returned by the :key
;;; function (if supplied). If :key is supplied, its argument is a list-1 or list-2 element. The :key function
;;; typically returns part of the supplied element. If :key is not supplied, the list-1 or list-2 element is used.
;;; An element of list-1 appears in the result if and only if it does not match any element of list-2.
;;; Parameters:
;;;   list-1 => a proper list.
;;;   list-2 => a proper list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   result-list => a list.
;;;
(defun nset-difference (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  "Returns a list of elements of list-1 that do not appear in list-2."
  (declare (system::%java-class-name "jcl.lists.functions.NsetDifference"))
  (labels ((nset-difference-aux (list1 list2 res)
             (if (consp list1)
                 (progn (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
                            (steve-splice list1 res)
                          (setq list1 (cdr list1)))
                        (nset-difference-aux list1 list2 res))
               res)))
    (nset-difference-aux list1 list2 nil)))

;;; SET-EXCLUSIVE-OR returns a list of elements that appear in exactly one of list-1 and list-2. For all possible
;;; ordered pairs consisting of one element from list-1 and one element from list-2, the :test or :test-not
;;; function is used to determine whether they satisfy the test. The first argument to the :test or :test-not
;;; function is the part of an element of list-1 that is returned by the :key function (if supplied); the second
;;; argument is the part of an element of list-2 that is returned by the :key function (if supplied). If :key is
;;; supplied, its argument is a list-1 or list-2 element. The :key function typically returns part of the supplied
;;; element. If :key is not supplied, the list-1 or list-2 element is used. The result contains precisely those
;;; elements of list-1 and list-2 that appear in no matching pair.
;;; Parameters:
;;;   list-1 => a proper list.
;;;   list-2 => a proper list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   result-list => a list.
;;;
(defun set-exclusive-or (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  "Returns a list of elements that appear in exactly one of list-1 and list-2."
  (declare (system::%java-class-name "jcl.lists.functions.SetExclusiveOr"))
  (labels ((set-exclusive-or-aux (list1 list2 res)
             (let ((elt (car list1)))
               (if (consp list1)
                   (progn (unless (with-set-keys (member (apply-key key elt) list2))
                            (setq res (nconc (list elt) res)))
                          (set-exclusive-or-aux (cdr list1) list2 res))
                 res))))
    (nconc (set-exclusive-or-aux list1 list2 nil) (set-exclusive-or-aux list2 list1 nil))))

;;; NSET-EXCLUSIVE-OR returns a list of elements that appear in exactly one of list-1 and list-2. It is destructive and
;;; it may destroy list-1. For all possible ordered pairs consisting of one element from list-1 and one element
;;; from list-2, the :test or :test-not function is used to determine whether they satisfy the test. The first
;;; argument to the :test or :test-not function is the part of an element of list-1 that is returned by the :key
;;; function (if supplied); the second argument is the part of an element of list-2 that is returned by the :key
;;; function (if supplied). If :key is supplied, its argument is a list-1 or list-2 element. The :key function
;;; typically returns part of the supplied element. If :key is not supplied, the list-1 or list-2 element is used.
;;; The result contains precisely those elements of list-1 and list-2 that appear in no matching pair.
;;; Parameters:
;;;   list-1 => a proper list.
;;;   list-2 => a proper list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   result-list => a list.
;;;
(defun nset-exclusive-or (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  "Returns a list of elements that appear in exactly one of list-1 and list-2."
  (declare (system::%java-class-name "jcl.lists.functions.NsetExclusiveOr"))
  (labels ((nset-exclusive-or-aux (list1 list2 res)
             (if (consp list1)
                 (progn (if (not (with-set-keys (member (apply-key key elt) list2)))
                            (steve-splice list1 res)
                          (setq list1 (cdr list1)))
                        (nset-exclusive-or-aux list1 list2 res))
               res)))
    (nconc (nset-exclusive-or-aux list1 list2 nil) (nset-exclusive-or-aux list2 list1 nil))))
|#
#|
(defun subsetp (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  (dolist (elt list1)
    (unless (with-set-keys (member (apply-key key elt) list2))
      (return-from subsetp nil)))
  t)
|#
(defun subsetp-test-not (list1 list2 test-not)
  (if (null list1)
      t
    (when (member (apply-key key (car list1)) list2 :key key :test-not test-not)
      (subsetp-aux (cdr list1) list2 test))))

(defun subsetp-test (list1 list2 test)
  (if (null list1)
      t
    (when (member (apply-key key (car list1)) list2 :key key :test test)
      (subsetp-aux (cdr list1) list2 test))))

;;; SUBSETP returns true if every element of list-1 matches some element of list-2, and false otherwise.
;;; Parameters:
;;;   list-1 => a proper list.
;;;   list-2 => a proper list.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   generalized-boolean => a generalized boolean.
;;;
(defun subsetp (list1 list2 &key key (test #'eql test-p) (test-not nil test-not-p))
  "Returns true if every element of list-1 matches some element of list-2, and false otherwise."
  (declare (system::%java-class-name "jcl.lists.functions.Subsetp"))
  (if test-not-p
      (subsetp-test-not list1 list2 test-not)
    (subsetp-test list1 list2 test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SUBST/NSUBST FAMILY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SUBST searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
;;; and performs substitution operations on tree. It makes a copy of tree, substituting new for every subtree or leaf of
;;; tree (whether the subtree or leaf is a car or a cdr of its parent) such that old and the subtree or leaf satisfy the
;;; test. If the functions succeeds, a new copy of the tree is returned in which each occurrence of such an element is
;;; replaced by the new element or subexpression. If no changes are made, the original tree may be returned.
;;; Parameters:
;;;   new => an object.
;;;   old => an object.
;;;   tree => a tree.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   new-tree => a tree.
;;;
(defun subst (new old tree &key key (test #'eql test-p) (test-not nil test-not-p))
  "Searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
   and performs substitution operations on tree."
  (declare (system::%java-class-name "jcl.lists.functions.Subst"))
  (labels ((s (subtree)
             (cond ((satisfies-the-test old subtree) new)
                   ((atom subtree) subtree)
                   (t (let ((car (s (car subtree)))
                            (cdr (s (cdr subtree))))
                        (if (and (eq car (car subtree))
                                 (eq cdr (cdr subtree)))
                            subtree
                          (cons car cdr)))))))
    (s tree)))

;;; SUBST-IF searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
;;; and performs substitution operations on tree. It makes a copy of tree, substituting new for every subtree or leaf of
;;; tree (whether the subtree or leaf is a car or a cdr of its parent) such that old and the subtree or leaf satisfy the
;;; test. If the functions succeeds, a new copy of the tree is returned in which each occurrence of such an element is
;;; replaced by the new element or subexpression. If no changes are made, the original tree may be returned.
;;; Parameters:
;;;   new => an object.
;;;   predicate => a symbol that names a function, or a function of one argument that returns a generalized boolean value.
;;;   tree => a tree.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   new-tree => a tree.
;;;
(defun subst-if (new predicate tree &key key)
  "Searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
   and performs substitution operations on tree."
  (declare (system::%java-class-name "jcl.lists.functions.SubstIf"))
  (labels ((s (subtree)
             (cond ((funcall predicate (apply-key key subtree)) new)
                   ((atom subtree) subtree)
                   (t (let ((car (s (car subtree)))
                            (cdr (s (cdr subtree))))
                        (if (and (eq car (car subtree))
                                 (eq cdr (cdr subtree)))
                            subtree
                          (cons car cdr)))))))
    (s tree)))

;;; SUBST-IF-NOT searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
;;; and performs substitution operations on tree. It makes a copy of tree, substituting new for every subtree or leaf of
;;; tree (whether the subtree or leaf is a car or a cdr of its parent) such that old and the subtree or leaf satisfy the
;;; test. If the functions succeeds, a new copy of the tree is returned in which each occurrence of such an element is
;;; replaced by the new element or subexpression. If no changes are made, the original tree may be returned.
;;; Parameters:
;;;   new => an object.
;;;   predicate => a symbol that names a function, or a function of one argument that returns a generalized boolean value.
;;;   tree => a tree.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   new-tree => a tree.
;;;
(defun subst-if-not (new predicate tree &key key)
  "Searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
   and performs substitution operations on tree."
  (declare (system::%java-class-name "jcl.lists.functions.SubstIfNot"))
  (labels ((s (subtree)
             (cond ((not (funcall predicate (apply-key key subtree))) new)
                   ((atom subtree) subtree)
                   (t (let ((car (s (car subtree)))
                            (cdr (s (cdr subtree))))
                        (if (and (eq car (car subtree))
                                 (eq cdr (cdr subtree)))
                            subtree
                          (cons car cdr)))))))
    (s tree)))
#|
(defun nsubst (new old tree &key key (test #'eql testp) (test-not nil notp))
  (labels ((s (subtree)
             (cond ((satisfies-the-test old subtree) new)
                   ((atom subtree) subtree)
                   (t (do* ((last nil subtree)
                            (subtree subtree (cdr subtree)))
                           ((atom subtree)
                            (when (satisfies-the-test old subtree)
                              (setf (cdr last) new)))
                        (if (satisfies-the-test old subtree)
                            (return (setf (cdr last) new))
                          (setf (car subtree) (s (car subtree)))))
                      subtree))))
     (s tree)))

;;; NSUBST searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
;;; and performs substitution operations on tree. The list structure of tree is altered by destructively replacing with
;;; new each leaf of the tree such that old and the leaf satisfy the test. If no changes are made, the original tree may
;;; be returned.
;;; Parameters:
;;;   new => an object.
;;;   old => an object.
;;;   tree => a tree.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   new-tree => a tree.
;;;
(defun nsubst (new old tree &key key (test #'eql test-p) (test-not nil test-not-p))
  "Searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
   and performs substitution operations on tree."
  (declare (system::%java-class-name "jcl.lists.functions.Nsubst"))
  (labels ((s-aux (last subtree)
             (if (atom subtree)
                 (when (satisfies-the-test old subtree)
                   (setf (cdr last) new))
               (cond ((satisfies-the-test old subtree)
                      (setf (cdr last) new))
                     (t
                      (setf (car subtree) (s (car subtree)))
                      (s-aux subtree (cdr subtree))))))
           (s (subtree)
             (cond ((satisfies-the-test old subtree) new)
                   ((atom subtree) subtree)
                   (t
                    (s-aux nil subtree)
                    subtree))))
    (s tree)))

(defun nsubst-if (new predicate tree &key key)
  (labels ((s (subtree)
             (cond ((funcall predicate (apply-key key subtree)) new)
                   ((atom subtree) subtree)
                   (t (do* ((last nil subtree)
                            (subtree subtree (cdr subtree)))
                           ((atom subtree)
                            (when (funcall predicate (apply-key key subtree))
                              (setf (cdr last) new)))
                        (if (funcall predicate (apply-key key subtree))
                            (return (setf (cdr last) new))
                          (setf (car subtree) (s (car subtree)))))
                      subtree))))
     (s tree)))

;;; NSUBST-IF searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
;;; and performs substitution operations on tree. The list structure of tree is altered by destructively replacing with
;;; new each leaf of the tree such that old and the leaf satisfy the test. If no changes are made, the original tree may
;;; be returned.
;;; Parameters:
;;;   new => an object.
;;;   predicate => a symbol that names a function, or a function of one argument that returns a generalized boolean value.
;;;   tree => a tree.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   new-tree => a tree.
;;;
(defun nsubst-if (new predicate tree &key key)
  "Searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
   and performs substitution operations on tree."
  (declare (system::%java-class-name "jcl.lists.functions.NsubstIf"))
  (labels ((s-aux (last subtree)
             (if (atom subtree)
                 (when (funcall predicate (apply-key key subtree))
                   (setf (cdr last) new))
               (cond ((funcall predicate (apply-key key subtree))
                      (setf (cdr last) new))
                     (t
                      (setf (car subtree) (s (car subtree)))
                      (s-aux subtree (cdr subtree))))))
           (s (subtree)
             (cond ((funcall predicate (apply-key key subtree)) new)
                   ((atom subtree) subtree)
                   (t
                    (s-aux nil subtree)
                    subtree))))
    (s tree)))

(defun nsubst-if-not (new predicate tree &key key)
  (labels ((s (subtree)
             (cond ((not (funcall predicate (apply-key key subtree))) new)
                   ((atom subtree) subtree)
                   (t (do* ((last nil subtree)
                            (subtree subtree (cdr subtree)))
                           ((atom subtree)
                            (when (not (funcall predicate (apply-key key subtree)))
                              (setf (cdr last) new)))
                        (if (not (funcall predicate (apply-key key subtree)))
                            (return (setf (cdr last) new))
                          (setf (car subtree) (s (car subtree)))))
                      subtree))))
    (s tree)))

;;; NSUBST-IF-NOT searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
;;; and performs substitution operations on tree. The list structure of tree is altered by destructively replacing with
;;; new each leaf of the tree such that old and the leaf satisfy the test. If no changes are made, the original tree may
;;; be returned.
;;; Parameters:
;;;   new => an object.
;;;   predicate => a symbol that names a function, or a function of one argument that returns a generalized boolean value.
;;;   tree => a tree.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   new-tree => a tree.
;;;
(defun nsubst-if-not (new predicate tree &key key)
  "Searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test
   and performs substitution operations on tree."
  (declare (system::%java-class-name "jcl.lists.functions.NsubstIfNot"))
  (labels ((s-aux (last subtree)
             (if (atom subtree)
                 (when (not (funcall predicate (apply-key key subtree)))
                   (setf (cdr last) new))
               (cond ((not (funcall predicate (apply-key key subtree)))
                      (setf (cdr last) new))
                     (t
                      (setf (car subtree) (s (car subtree)))
                      (s-aux subtree (cdr subtree))))))
           (s (subtree)
             (cond ((not (funcall predicate (apply-key key subtree))) new)
                   ((atom subtree) subtree)
                   (t
                    (s-aux nil subtree)
                    subtree))))
    (s tree)))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SUBLIS FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SUBLIS makes substitutions for objects in tree (a structure of conses). It looks at all subtrees and leaves of tree;
;;; if a subtree or leaf appears as a key in alist (that is, the key and the subtree or leaf satisfy the test), it is
;;; replaced by the object with which that key is associated. This operation is non-destructive. If it succeeds, a new
;;; copy of tree is returned in which each occurrence of such a subtree or leaf is replaced by the object with which it
;;; is associated. If no changes are made, the original tree is returned.
;;; Parameters:
;;;   alist => an association list.
;;;   tree => a tree.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   new-tree => a tree.
;;;
(defun sublis (alist tree &key key (test #'eql test-p) (test-not nil test-not-p))
  "Makes substitutions for objects in tree (a structure of conses). It looks at all subtrees and leaves of tree;
   if a subtree or leaf appears as a key in alist (that is, the key and the subtree or leaf satisfy the test), it is
   replaced by the object with which that key is associated."
  (declare (system::%java-class-name "jcl.lists.functions.Sublis"))
  (labels ((s (subtree)
            (let* ((key-val (apply-key key subtree))
                   (assoc (if test-not-p
                              (assoc key-val alist :test-not test-not)
                            (assoc key-val alist :test test))))
              (cond (assoc (cdr assoc))
                    ((atom subtree) subtree)
                    (t (let ((car (s (car subtree)))
                             (cdr (s (cdr subtree))))
                         (if (and (eq car (car subtree))
                                  (eq cdr (cdr subtree)))
                             subtree
                           (cons car cdr))))))))
    (s tree)))
#|
(defmacro nsublis-macro ()
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key subtree)))
      (if test-not-p
	      (assoc ,key-tmp alist :test-not test-not)
	    (assoc ,key-tmp alist :test test)))))

(defun nsublis (alist tree &key key (test #'eql) (test-not nil notp))
  "Substitutes new for subtrees matching old."
  (let (temp)
    (labels ((s (subtree)
              (cond ((setq temp (nsublis-macro)) (cdr temp))
                    ((atom subtree) subtree)
                    (t (do* ((last nil subtree)
                             (subtree subtree (cdr subtree)))
                            ((atom subtree)
                             (when (setq temp (nsublis-macro))
                               (setf (cdr last) (cdr temp))))
                         (if (setq temp (nsublis-macro))
                             (return (setf (cdr last) (cdr temp)))
                           (setf (car subtree) (s (car subtree)))))
                       subtree))))
      (s tree))))

;;; NSUBLIS makes substitutions for objects in tree (a structure of conses). It looks at all subtrees and leaves of tree;
;;; if a subtree or leaf appears as a key in alist (that is, the key and the subtree or leaf satisfy the test), it is
;;; replaced by the object with which that key is associated. This operation may be destructive. If it succeeds, tree is
;;; returned in which each occurrence of such a subtree or leaf is replaced by the object with which it is associated.
;;; If no changes are made, the original tree is returned.
;;; Parameters:
;;;   alist => an association list.
;;;   tree => a tree.
;;;   test, test-not => a designator for a function of two arguments that returns a generalized boolean.
;;;   key => a designator for a function of one argument, or nil.
;;; Returns:
;;;   new-tree => a tree.
;;;
(defun nsublis (alist tree &key key (test #'eql test-p) (test-not nil test-not-p))
  "Makes substitutions for objects in tree (a structure of conses). It looks at all subtrees and leaves of tree;
   if a subtree or leaf appears as a key in alist (that is, the key and the subtree or leaf satisfy the test), it is
   replaced by the object with which that key is associated."
  (declare (system::%java-class-name "jcl.lists.functions.Nsublis"))
  (let (temp)
    (labels ((s-aux (last subtree)
               (cond ((atom subtree)
                       (when (setq temp (nsublis-macro))
                         (setf (cdr last) (cdr temp))))
                     ((setq temp (nsublis-macro))
                      (setf (cdr last) (cdr temp)))
                     (t
                      (setf (car subtree) (s (car subtree)))
                      (s-aux subtree (cdr subtree)))))
             (s (subtree)
              (cond ((setq temp (nsublis-macro)) (cdr temp))
                    ((atom subtree) subtree)
                    (t (s-aux nil subtree) subtree))))
      (s tree))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defmacro push (value place &environment env)
  "Takes an object and a location holding a list.  Conses the object onto
  the list, returning the modified list.  OBJ is evaluated before PLACE."
  (declare (system::%java-class-name "jcl.lists.functions.Push"))
  (multiple-value-bind (result ok)
                       (macroexpand place env)
    (if (symbolp result)
      `(setq ,result (cons ,value ,result))
      (let ((val-tmp (gensym "VAL-")))
        (multiple-value-bind (temps vals stores store-form access-form)
                             (get-setf-expansion result env)
          (let ((tmps (cons val-tmp temps))
                (vals (cons value vals))
                (store (gensym "Store-")))
            `(let* (,@(mapcar #'list tmps vals))
               (let ((,(first stores) (cons ,val-tmp ,access-form)))
                 ,store-form
                 ,(first stores)))))))))

(defmacro pushnew (value place &rest keys &environment env)
  "Takes an object and a location holding a list.  If the object is already
  in the list, does nothing.  Else, conses the object onto the list.  Returns
  NIL.  If there is a :TEST keyword, this is used for the comparison."
  (declare (system::%java-class-name "jcl.lists.functions.Pushnew"))

  (multiple-value-bind (result ok)
                       (macroexpand place env)
    (if (symbolp place)
      `(setq ,result (adjoin ,value ,result ,@keys))
      (multiple-value-bind (temps vals stores store-form access-form)
                           (get-setf-expansion result env)
        (cond ((cdr stores)
               (let ((g (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) (rest value))))
                 `(multiple-value-bind ,g ,value
                    (let* (,@(mapcar #'list temps vals))
                      (multiple-value-bind ,stores
                                           (values ,@(mapcar #'(lambda (a b) `(adjoin ,a ,b ,@keys))
                                                             g (rest access-form)))
                        ,store-form)))))
	      (t (let ((g (gensym)))
                   `(let* ((,g ,value)
                           ,@(mapcar #'list temps vals)
                           (,@stores (adjoin ,g ,access-form ,@keys)))
                      ,store-form
                      ,(first stores)))))))))

(defmacro pop (place &environment env)
  "The argument is a location holding a list.  Pops one item off the front
  of the list and returns it."
  (declare (system::%java-class-name "jcl.lists.functions.Pop"))

  (multiple-value-bind (result ok)
                       (macroexpand place env)
    (if (symbolp result)
      `(prog1 (car ,result)
  	      (setq ,result (cdr ,result)))
      (multiple-value-bind (dummies vals newval setter getter)
  	(get-setf-method result env)
  	(do* ((d dummies (cdr d))
	      (v vals (cdr v))
	      (let-list nil))
	     ((null d)
	      (push (list (car newval) getter) let-list)
	      `(let* ,(nreverse let-list)
		(prog1 (car ,(car newval))
		  (setq ,(car newval) (cdr ,(car newval)))
		  ,setter)))
	  (push (list (car d) (car v)) let-list))))))
|#
#|
(defmacro pop (place &environment env)
  "The argument is a location holding a list.  Pops one item off the front
  of the list and returns it."
  (declare (system::%java-class-name "jcl.lists.functions.Pop"))
  (let ((val-tmp (gensym "VAL-")))
    (multiple-value-bind (temps vals stores store-form access-form)
                         (get-setf-expansion place env)
      (let ((store (gensym "Store-")))
        `(let* ,(mapcar #'list temps vals)
           (let ((,(first stores) ,access-form))
             (prog1
               (car,(first stores))
               (setq ,(first stores) (cdr ,(first stores)))
               ,store-form)))))))
|#
#|
(defmacro remf (place indicator &environment env)
  "Place may be any place expression acceptable to SETF, and is expected
  to hold a property list or ().  This list is destructively altered to
  remove the property specified by the indicator.  Returns T if such a
  property was present, NIL if not."
  (declare (system::%java-class-name "jcl.lists.functions.Remf"))
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-expansion place env)
    (do* ((d dummies (cdr d))
	  (v vals (cdr v))
	  (let-list nil)
	  (ind-temp (gensym))
	  (local1 (gensym))
	  (local2 (gensym)))
	 ((null d)
	  (push (list ind-temp indicator) let-list)
	  (push (list (car newval) getter) let-list)
	  `(let* ,(system::%nreverse let-list)
	     (do ((,local1 ,(car newval) (cddr ,local1))
		  (,local2 nil ,local1))
		 ((atom ,local1) nil)
	       (cond ((atom (cdr ,local1))
		      (error "Odd-length property list in REMF."))
		     ((eq (car ,local1) ,ind-temp)
		      (cond (,local2
			     (rplacd (cdr ,local2) (cddr ,local1))
			     (return t))
			    (t (setq ,(car newval) (cddr ,(car newval)))
			       ,setter
			       (return t))))))))
      (push (list (car d) (car v)) let-list))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defmacro push (&environment env item place)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (cons ,item ,place))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (let ((g (gensym)))
          `(let* ((,g ,item)
                  ,@(mapcar #'list dummies vals)
                  (,(car newval) (cons ,g ,getter)))
             ,setter)))))

(defmacro pushnew (&environment env item place &rest keys)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (adjoin ,item ,place ,@keys))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (let ((g (gensym)))
          `(let* ((,g ,item)
                  ,@(mapcar #'list dummies vals)
                  (,(car newval) (adjoin ,g ,getter ,@keys)))
             ,setter)))))

(defmacro pop (&environment env place)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(prog1 (car ,place)
	      (setq ,place (cdr ,place)))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (do* ((d dummies (cdr d))
              (v vals (cdr v))
              (let-list nil))
             ((null d)
              (push (list (car newval) getter) let-list)
              `(let* ,(nreverse let-list)
                 (prog1 (car ,(car newval))
                        (setq ,(car newval) (cdr ,(car newval)))
                        ,setter)))
          (push (list (car d) (car v)) let-list)))))

(defmacro remf (place indicator &environment env)
  "Place may be any place expression acceptable to SETF, and is expected
   to hold a property list or (). This list is destructively altered to
   remove the property specified by the indicator. Returns T if such a
   property was present, NIL if not."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (do* ((d dummies (cdr d))
	  (v vals (cdr v))
	  (let-list nil)
	  (ind-temp (gensym))
	  (local1 (gensym))
	  (local2 (gensym)))
	 ((null d)
          ;; See ANSI 5.1.3 for why we do out-of-order evaluation
	  (push (list ind-temp indicator) let-list)
	  (push (list (car newval) getter) let-list)
	  `(let* ,(nreverse let-list)
	     (do ((,local1 ,(car newval) (cddr ,local1))
		  (,local2 nil ,local1))
		 ((atom ,local1) nil)
	       (cond ((atom (cdr ,local1))
		      (error "Odd-length property list in REMF."))
		     ((eq (car ,local1) ,ind-temp)
		      (cond (,local2
			     (rplacd (cdr ,local2) (cddr ,local1))
			     (return t))
			    (t (setq ,(car newval) (cddr ,(car newval)))
			       ,setter
			       (return t))))))))
      (push (list (car d) (car v)) let-list))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defmacro push (obj place &environment env)
  "Takes an object and a location holding a list.  Conses the object onto
  the list, returning the modified list.  OBJ is evaluated before PLACE."

  ;; This special case for place being a symbol isn't strictly needed.
  ;; It's so we can do push (and pushnew) with a kernel.core.
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (cons ,obj ,place))
      (multiple-value-bind (dummies vals newval setter getter)
	  (get-setf-expansion place env)
	(cond
	  ((cdr newval)
	   ;; Handle multiple values
	   (let ((g (mapcar #'(lambda (x)
				(declare (ignore x))
				(gensym))
			    (rest obj))))
	     `(multiple-value-bind ,g
		  ,obj
		(let* (,@(mapcar #'list dummies vals))
		  (multiple-value-bind ,newval
		      (values ,@(mapcar #'(lambda (a b)
					     (list 'cons a b))
					 g (rest getter)))
		    ,setter)))))
	  (t
	   ;; A single value
	   (let ((g (gensym)))
	     `(let* ((,g ,obj)
		     ,@(mapcar #'list dummies vals)
		     (,@newval (cons ,g ,getter)))
	       ,setter)))))))

(defmacro pushnew (obj place &rest keys &environment env)
  "Takes an object and a location holding a list.  If the object is already
  in the list, does nothing.  Else, conses the object onto the list.  Returns
  NIL.  If there is a :TEST keyword, this is used for the comparison."
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (adjoin ,obj ,place ,@keys))
      (multiple-value-bind (vars vals stores setter getter)
	  (get-setf-expansion place env)
	(cond
	  ((cdr stores)
	   ;; Multiple values
	   (let ((g (mapcar #'(lambda (x)
				(declare (ignore x))
				(gensym))
			    (rest obj))))
	     `(multiple-value-bind ,g
		  ,obj
		(let* (,@(mapcar #'list vars vals))
		  (multiple-value-bind ,stores
		      (values ,@(mapcar #'(lambda (a b)
					    `(adjoin ,a ,b ,@keys))
					g (rest getter)))
		  ,setter)))))
	  (t
	   ;; Single value
	   (let ((g (gensym)))
	     `(let* ((,g ,obj)
		     ,@(mapcar #'list vars vals)
		     (,@stores (adjoin ,g ,getter ,@keys)))
		,setter)))))))

(defmacro pop (place &environment env)
  "The argument is a location holding a list.  Pops one item off the front
  of the list and returns it."
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(prog1 (car ,place)
	      (setq ,place (cdr ,place)))
      (multiple-value-bind (dummies vals newval setter getter)
	  (get-setf-method place env)
	(do* ((d dummies (cdr d))
	      (v vals (cdr v))
	      (let-list nil))
	     ((null d)
	      (push (list (car newval) getter) let-list)
	      `(let* ,(nreverse let-list)
		(prog1 (car ,(car newval))
		  (setq ,(car newval) (cdr ,(car newval)))
		  ,setter)))
	  (push (list (car d) (car v)) let-list)))))

(defmacro remf (place indicator &environment env)
  "Place may be any place expression acceptable to SETF, and is expected
  to hold a property list or ().  This list is destructively altered to
  remove the property specified by the indicator.  Returns T if such a
  property was present, NIL if not."
  (multiple-value-bind (dummies vals newval setter getter)
		       (get-setf-method place env)
    (do* ((d dummies (cdr d))
	  (v vals (cdr v))
	  (let-list nil)
	  (ind-temp (gensym))
	  (local1 (gensym))
	  (local2 (gensym)))
	 ((null d)
	  ;; See ANSI 5.1.3 for why we do out-of-order evaluation
	  (push (list ind-temp indicator) let-list)
	  (push (list (car newval) getter) let-list)
	  `(let* ,(nreverse let-list)
	     (do ((,local1 ,(car newval) (cddr ,local1))
		  (,local2 nil ,local1))
		 ((atom ,local1) nil)
	       (cond ((atom (cdr ,local1))
		      (error (intl:gettext "Odd-length property list in REMF.")))
		     ((eq (car ,local1) ,ind-temp)
		      (cond (,local2
			     (rplacd (cdr ,local2) (cddr ,local1))
			     (return t))
			    (t (setq ,(car newval) (cddr ,(car newval)))
			       ,setter
			       (return t))))))))
      (push (list (car d) (car v)) let-list))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;

(export '(caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr
          caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr first second third fourth fifth sixth
          seventh eighth ninth tenth rest tree-equal member member-if member-if-not adjoin assoc assoc-if assoc-if-not
          rassoc rassoc-if rassoc-if-not mapc mapcar mapcan mapl maplist mapcon intersection nintersection union nunion
          set-difference nset-difference set-exclusive-or nset-exclusive-or subsetp subst subst-if subst-if-not nsubst
          nsubst-if nsubst-if-not sublis nsublis push pushnew pop remf)
        "COMMON-LISP")
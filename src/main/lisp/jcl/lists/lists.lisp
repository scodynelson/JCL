;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

(defun caaaar (lst)
  "Returns the first object of the caaar of a list. Ex: (caaaar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => A"
  (declare (system::%java-class-name "jcl.lists.functions.Caaaar"))
  (car (car (car (car lst)))))

(defun caaadr (lst)
  "Returns the first object of the caadr of a list. Ex: (caaadr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => K"
  (declare (system::%java-class-name "jcl.lists.functions.Caaadr"))
  (car (car (car (cdr lst)))))

(defun caaar (lst)
  "Returns the first object in the caar of a list. Ex: (caaar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (A B)"
  (declare (system::%java-class-name "jcl.lists.functions.Caaar"))
  (car (car (car lst))))

(defun caadar (lst)
  "Returns the first object of the cadar of a list. Ex: (caadar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => F"
  (declare (system::%java-class-name "jcl.lists.functions.Caadar"))
  (car (car (cdr (car lst)))))

(defun caaddr (lst)
  "Returns the first object of the caddr of a list. Ex: (caaddr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => O"
  (declare (system::%java-class-name "jcl.lists.functions.Caaddr"))
  (car (car (cdr (cdr lst)))))

(defun caadr (lst)
  "Returns the first object in the cadr of a list. Ex: (caadr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (K L)"
  (declare (system::%java-class-name "jcl.lists.functions.Caadr"))
  (car (car (cdr lst))))

(defun caar (lst)
  "Returns the first object of the car of a list. Ex: (caar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => ((A B) C D E)"
  (declare (system::%java-class-name "jcl.lists.functions.Caar"))
  (car (car lst)))

(defun cadaar (lst)
  "Returns the first object of the cdaar of a list. Ex: (cadaar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => C"
  (declare (system::%java-class-name "jcl.lists.functions.Cadaar"))
  (car (cdr (car (car lst)))))

(defun cadadr (lst)
  "Returns the first object of the cdadr of a list. Ex: (cadadr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => M"
  (declare (system::%java-class-name "jcl.lists.functions.Cadadr"))
  (car (cdr (car (cdr lst)))))

(defun cadar (lst)
  "Returns the first object of the cdar of a list. Ex: (cadar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (F G)"
  (declare (system::%java-class-name "jcl.lists.functions.Cadar"))
  (car (cdr (car lst))))

(defun caddar (lst)
  "Returns the first object of the cddar of a list. Ex: (caddar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (H)"
  (declare (system::%java-class-name "jcl.lists.functions.Caddar"))
  (car (cdr (cdr (car lst)))))

(defun cadddr (lst)
  "Returns the first object of the cdddr of a list. Ex: (cadddr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (Q)"
  (declare (system::%java-class-name "jcl.lists.functions.Cadddr"))
  (car (nthcdr 3 lst)))

(defun caddr (lst)
  "Returns the first object in the cddr of a list. Ex: (caddr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (O P)"
  (declare (system::%java-class-name "jcl.lists.functions.Caddr"))
  (car (cdr (cdr lst))))

(defun cadr (lst)
  "Returns the first object of the cdr of a list. Ex: (cadr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => ((K L) M N)"
  (declare (system::%java-class-name "jcl.lists.functions.Cadr"))
  (car (cdr lst)))

(defun cdaaar (lst)
  "Returns the cdr of the caaar of a list. Ex: (cdaaar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (B)"
  (declare (system::%java-class-name "jcl.lists.functions.Cdaaar"))
  (cdr (car (car (car lst)))))

(defun cdaadr (lst)
  "Returns the cdr of the caadr of a list. Ex: (cdaadr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (L)"
  (declare (system::%java-class-name "jcl.lists.functions.Cdaadr"))
  (cdr (car (car (cdr lst)))))

(defun cdaar (lst)
  "Returns the cdr of the caar of a list. Ex: (cdaar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (C D E)"
  (declare (system::%java-class-name "jcl.lists.functions.Cdaar"))
  (cdr (car (car lst))))

(defun cdadar (lst)
  "Returns the cdr of the cadar of a list. Ex: (cdadar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (G)"
  (declare (system::%java-class-name "jcl.lists.functions.Cdadar"))
  (cdr (car (cdr (car lst)))))

(defun cdaddr (lst)
  "Returns the cdr of the caddr of a list. Ex: (cdaddr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (P)"
  (declare (system::%java-class-name "jcl.lists.functions.Cdaddr"))
  (cdr (car (cdr (cdr lst)))))

(defun cdadr (lst)
  "Returns the cdr of the cadr of a list. Ex: (cdadr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (M N)"
  (declare (system::%java-class-name "jcl.lists.functions.Cdadr"))
  (cdr (car (cdr lst))))

(defun cdar (lst)
  "Returns the cdr of the first object in a list. Ex: (cdar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => ((F G) (H) I J)"
  (declare (system::%java-class-name "jcl.lists.functions.Cdar"))
  (cdr (car lst)))

(defun cddaar (lst)
  "Returns the cdr of the cdaar of a list. Ex: (cddaar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (D E)"
  (declare (system::%java-class-name "jcl.lists.functions.Cddaar"))
  (cdr (cdr (car (car lst)))))

(defun cddadr (lst)
  "Returns the cdr of the cdadr of a list. Ex: (cddadr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (N)"
  (declare (system::%java-class-name "jcl.lists.functions.Cddadr"))
  (cdr (cdr (car (cdr lst)))))

(defun cddar (lst)
  "Returns the cdr of the cdar of a list. Ex: (cddar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => ((H) I J)"
  (declare (system::%java-class-name "jcl.lists.functions.Cddar"))
  (cdr (cdr (car lst))))

(defun cdddar (lst)
  "Returns the cdr of the cddar of a list. Ex: (cdddar `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (I J)"
  (declare (system::%java-class-name "jcl.lists.functions.Cdddar"))
  (cdr (cdr (cdr (car lst)))))

(defun cddddr (lst)
  "Returns the cdr of the cdddr of a list. Ex: (cddddr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R)) => (R)"
  (declare (system::%java-class-name "jcl.lists.functions.Cddddr"))
  (cdr (nthcdr 3 lst)))

(defun cdddr (lst)
  "Returns the cdr of the cddr of a list. Ex: (cdddr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R) ) => ((Q) R)"
  (declare (system::%java-class-name "jcl.lists.functions.Cdddr"))
  (cdr (cdr (cdr lst))))

(defun cddr (lst)
  "Returns the cdr of all but the fist item in a list. Ex: (cddr `((((A B) C D E) (F G) (H) I J) ((K L) M N) (O P) (Q) R) ) => ((O P) (Q) R)"
  (declare (system::%java-class-name "jcl.lists.functions.Cddr"))
  (cdr (cdr lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; NUMERICAL ACCESSOR FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun first (lst)
  "Returns the 1st object in a list or NIL if the list is empty."
  (declare (system::%java-class-name "jcl.lists.functions.First"))
  (car lst))

(defun second (lst)
  "Returns the 2nd object in a list or NIL if there is not 2nd object."
  (declare (system::%java-class-name "jcl.lists.functions.Second"))
  (car (rest lst)))

(defun third (lst)
  "Returns the 3rd object in a list or NIL if there is not 3rd object."
  (declare (system::%java-class-name "jcl.lists.functions.Third"))
  (car (nthcdr 2 lst)))

(defun fourth (lst)
  "Returns the 4th object in a list or NIL if there is not 4th object."
  (declare (system::%java-class-name "jcl.lists.functions.Fourth"))
  (car (nthcdr 3 lst)))

(defun fifth (lst)
  "Returns the 5th object in a list or NIL if there is not 5th object."
  (declare (system::%java-class-name "jcl.lists.functions.Fifth"))
  (car (nthcdr 4 lst)))

(defun sixth (lst)
  "Returns the 6th object in a list or NIL if there is not 6th object."
  (declare (system::%java-class-name "jcl.lists.functions.Sixth"))
  (car (nthcdr 5 lst)))

(defun seventh(lst)
  "Returns the 7th object in a list or NIL if there is not 7th object."
  (declare (system::%java-class-name "jcl.lists.functions.Seventh"))
  (car (nthcdr 6 lst)))

(defun eighth (lst)
  "Returns the 8th object in a list or NIL if there is not 8th object."
  (declare (system::%java-class-name "jcl.lists.functions.Eighth"))
  (car (nthcdr 7 lst)))

(defun ninth (lst)
  "Returns the 9th object in a list or NIL if there is not 9th object."
  (declare (system::%java-class-name "jcl.lists.functions.Ninth"))
  (car (nthcdr 8 lst)))

(defun tenth (lst)
  "Returns the 10th object in a list or NIL if there is not 10th object."
  (declare (system::%java-class-name "jcl.lists.functions.Tenth"))
  (car (nthcdr 9 lst)))

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

(defmacro assoc-guts (test-guy)
  `(labels ((assoc-guts-aux (alist)
             (if (not (endp alist))
                 (if (car alist)
                     (if ,test-guy
                         (car alist)
                       (assoc-guts-aux (cdr alist)))))))
    (assoc-guts-aux alist)))

(defmacro with-set-keys (funcall)
  `(cond ((and test-p test-not-p) (error "Test and test-not both supplied."))
	 (test-not-p ,(append funcall '(:key key :test-not test-not)))
	 (t ,(append funcall '(:key key :test test)))))

(defmacro satisfies-the-test (item elt)
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key ,elt)))
      (cond (test-p (funcall test ,item ,key-tmp))
	    (test-not-p (not (funcall test-not ,item ,key-tmp)))
	    (t (funcall test ,item ,key-tmp))))))

(defmacro steve-splice (source destination)
  `(let ((temp ,source))
     (setf ,source (cdr ,source)
	   (cdr temp) ,destination
	   ,destination temp)))

(defmacro nsublis-macro ()
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key subtree)))
      (if test-not-p
	  (assoc ,key-tmp alist :test-not test-not)
	(assoc ,key-tmp alist :test test)))))








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
(defun tree-equal (tree1 tree2 &key (test #'eql test-p) (test-not nil test-not-p))
  "Tests whether two trees are of the same shape and have the same leaves. It returns true if tree-1 and tree-2
   are both atoms and satisfy the test, or if they are both conses and the car of tree-1 is tree-equal to the car of tree-2
   and the cdr of tree-1 is tree-equal to the cdr of tree-2. Otherwise, it returns false."
  (declare (system::%java-class-name "jcl.lists.functions.TreeEqual"))
  (let ((%test (if test-not-p #'(lambda (x y) (not (funcall test-not x y))) test)))
    (labels ((tree-equal-aux (tree1 tree2 real-test)
               (cond ((consp tree1)
                      (and (consp tree2)
                           (tree-equal-aux (car tree1) (car tree2) real-test)
                           (tree-equal-aux (cdr tree1) (cdr tree2) real-test)))
                     ((consp tree2) nil)
                     ((funcall real-test tree1 tree2) t)
                     (t ()))))
      (tree-equal-aux tree1 tree2 %test))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MEMBER FAMILY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
               (let ((car (car list)))
                 (if (satisfies-the-test item car)
                     list
                   (member-aux (cdr list)))))))
    (member-aux list)))

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
             (if (endp list)
                 nil
               (if (funcall predicate (apply-key key (car list)))
                   list
                 (member-if-aux (cdr list))))))
    (member-if-aux list)))

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
             (if (endp list)
                 nil
               (if (not (funcall predicate (apply-key key (car list))))
                   list
                 (member-if-not-aux (cdr list))))))
    (member-if-not-aux list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ASSOC/RASSOC FAMILY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defun assoc (item alist &key key (test #'eql testp) (test-not nil not-testp))
  "Returns the first cons in alist whose car satisfies the test, or nil if no such cons is found.
   If nil appears in alist in place of a pair, it is ignored."
  (declare (system::%java-class-name "jcl.lists.functions.Assoc"))
  (declare (list alist))
  (cond (test (if key
                  (assoc-guts (funcall test item (funcall key (caar alist))))
                (assoc-guts (funcall test item (caar alist)))))
	(test-not (if key
                      (assoc-guts (not (funcall test-not item (funcall key (caar alist)))))
                    (assoc-guts (not (funcall test-not item (caar alist))))))
	(t (if key
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
  (declare (list alist))
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
  (declare (list alist))
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
(defun rassoc (item alist &key key (test #'eql testp) (test-not nil not-testp))
  "Returns the first cons in alist whose cdr satisfies the test, or nil if no such cons is found.
   If nil appears in alist in place of a pair, it is ignored."
  (declare (system::%java-class-name "jcl.lists.functions.Rassoc"))
  (declare (list alist))
  (cond (test (if key
                  (assoc-guts (funcall test item (funcall key (cdar alist))))
                (assoc-guts (funcall test item (cdar alist)))))
	(test-not (if key
                      (assoc-guts (not (funcall test-not item (funcall key (cdar alist)))))
                    (assoc-guts (not (funcall test-not item (cdar alist))))))
	(t (if key
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
  (declare (list alist))
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
  (declare (list alist))
  (if key
      (assoc-guts (not (funcall predicate (funcall key (cdar alist)))))
    (assoc-guts (not (funcall predicate (cdar alist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MAP FAMILY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mapx-aux-helper (function1 original-arglists accumulate take-car)
  "This function is called by MAPC, MAPCAR, MAPCAN, MAPL, MAPLIST, and MAPCON.
   It Maps function over the arglists in the appropriate way. It is done when any
   of the arglists runs out.  Until then, it CDRs down the arglists calling the
   function and accumulating results as desired."
  (labels ((mapx-aux-1 (function1 accumulate take-car arglists ret-list temp res &optional original-arglists)
             (labels ((mapx-aux-2 (arglists args take-car)
                        (let ((l arglists))
                          (cond ((not (null l))
                                 (setq args (cons (if take-car
                                                      (caar l)
                                                    (car l)) args))
                                 (rplaca l (cdar l))
                                 (setq args (mapx-aux-2 (cdr l) args take-car))))
                          args))
                      (mapx-aux-3 (arglists)
                        (let ((x (car arglists)))
                          (if (equal nil x)
                              t
                            (if (not (equal x (car (last arglists))))
                                (mapx-aux-3 (cdr arglists)))))))
               (let ((args '()))
                 (cond ((equal nil (mapx-aux-3 arglists))
                        (setq args (mapx-aux-2 arglists args take-car))
                        (setq res (apply function1 (system::%nreverse args)))
                        (case accumulate
                          (:nconc (setq temp (last (nconc temp res))))
                          (:list (rplacd temp (list res))
                           (setq temp (cdr temp))))
                        (mapx-aux-1 function1 accumulate take-car arglists ret-list temp res)))
                 (if original-arglists
                     (if accumulate
                         (cdr ret-list)
                       (car original-arglists)))))))
    (let* ((arglists (copy-list original-arglists))
           (ret-list (list nil))
           (temp ret-list)
           (res nil))
      (mapx-aux-1 function1 accumulate take-car arglists ret-list temp res original-arglists))))

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
(defun mapc (function1 &rest list)
  "Operates on successive elements of the lists, ie. function is applied to the first element of each list,
   then to the second element of each list, and so on. The iteration terminates when the shortest list runs out,
   and excess elements in other lists are ignored. Fhe results of applying function are not accumulated and the
   list argument is returned."
  (declare (system::%java-class-name "jcl.lists.functions.Mapc"))
  (mapx-aux-helper function1 list nil t))

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
(defun mapcar (function1 &rest list)
  "Operates on successive elements of the lists. Function is applied to the first element of each list,
   then to the second element of each list, and so on. The iteration terminates when the shortest list runs out,
   and excess elements in other lists are ignored. The value returned by mapcar is a list of the results of
   successive calls to function."
  (declare (system::%java-class-name "jcl.lists.functions.Mapcar"))
  (mapx-aux-helper function1 list :list t))

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
(defun mapcan (function1 &rest list)
  "Operates on successive elements of the lists, ie. function is applied to the first element of each list,
   then to the second element of each list, and so on. The iteration terminates when the shortest list runs out,
   and excess elements in other lists are ignored. The results of applying function are combined into a list by
   the use of nconc rather than list."
  (declare (system::%java-class-name "jcl.lists.functions.Mapcan"))
  (mapx-aux-helper function1 list :nconc t))

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
(defun mapl (function1 &rest list)
  "Operates on successive elements of the lists. Function is applied to successive sublists of the lists.
   Function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of
   the cdr of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements
   in other lists are ignored. The results of applying function are not accumulated and the list argument is returned."
  (declare (system::%java-class-name "jcl.lists.functions.Mapl"))
  (mapx-aux-helper function1 list nil nil))

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
(defun maplist (function1 &rest list)
  "Operates on successive elements of the lists. Function is applied to successive sublists of the lists.
   Function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of
   the cdr of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements
   in other lists are ignored. The value returned by maplist is a list of the results of successive calls to function."
  (declare (system::%java-class-name "jcl.lists.functions.Maplist"))
  (mapx-aux-helper function1 list :list nil))

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
(defun mapcon (function1 &rest list)
  "Operates on successive elements of the lists. Function is applied to successive sublists of the lists.
   Function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of
   the cdr of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements
   in other lists are ignored. The results of applying function are combined into a list by the use of nconc rather
   than list."
  (declare (system::%java-class-name "jcl.lists.functions.Mapcon"))
  (mapx-aux-helper function1 list :nconc nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; INTERSECTION/UNION FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
               (if (not (endp list1))
                   (progn (when (with-set-keys (member (apply-key key elt) list2))
                                (setq res (nconc (list elt) res)))
                          (intersection-aux (cdr list1) list2 res))
                 res))))
  (intersection-aux list1 list2 nil)))

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
  (declare (system::%java-class-name "jcl.lists.functions.NIntersection"))
  (labels ((nintersection-aux (list1 list2 res)
             (if (not (endp list1))
                 (progn (if (with-set-keys (member (apply-key key (car list1)) list2))
                            (steve-splice list1 res)
                          (setq list1 (cdr list1)))
                        (nintersection-aux list1 list2 res))
                 res)))
  (nintersection-aux list1 list2 nil)))

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
               (if (not (endp list1))
                   (progn (unless (with-set-keys (member (apply-key key elt) list2))
                                  (setq res (nconc (list elt) res)))
                          (union-aux (cdr list1) list2 res))
                 res))))
  (union-aux list1 list2 list2)))

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
  (declare (system::%java-class-name "jcl.lists.functions.NUnion"))
  (labels ((nunion-aux (list1 list2 res)
             (if (not (endp list1))
                 (progn (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
                            (steve-splice list1 res)
                          (setq list1 (cdr list1)))
                        (nunion-aux list1 list2 res))
                 res)))
  (nunion-aux list1 list2 list2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SET-DIFFERENCE/SET-EXCLUSIVE-OR FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
               (if (not (endp list1))
                   (progn (when (not (with-set-keys (member (apply-key key elt) list2)))
                                (setq res (nconc (list elt) res)))
                          (set-difference-aux (cdr list1) list2 res))
                 res))))
    (if (null list2)
        list1
      (set-difference-aux list1 list2 nil))))

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
  (declare (system::%java-class-name "jcl.lists.functions.NSetDifference"))
  (labels ((nset-difference-aux (list1 list2 res)
             (if (not (endp list1))
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
               (if (not (endp list1))
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
  (declare (system::%java-class-name "jcl.lists.functions.NSetExclusiveOr"))
  (labels ((nset-exclusive-or-aux (list1 list2 res)
             (if (not (endp list1))
                 (progn (if (not (with-set-keys (member (apply-key key elt) list2)))
                            (steve-splice list1 res)
                          (setq list1 (cdr list1)))
                   (nset-exclusive-or-aux list1 list2 res))
               res)))
  (nconc (nset-exclusive-or-aux list1 list2 nil) (nset-exclusive-or-aux list2 list1 nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SUBSETP FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let ((%test (if test-not-p #'(lambda (x y) (not (funcall test-not x y))) test)))
    (labels ((subsetp-aux (list1 list2 real-test)
               (if (null list1)
                   t
                 (when (member (if key (apply-key key (car list1)) (car list1)) list2 :key key :test real-test)
                   (subsetp-aux (cdr list1) list2 real-test)))))
      (subsetp-aux list1 list2 %test))))

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
  (declare (system::%java-class-name "jcl.lists.functions.NSubst"))
  (labels ((s-aux (last subtree)
             (if (atom subtree)
                 (if (satisfies-the-test old subtree)
                     (setf (cdr last) new))
               (if (satisfies-the-test old subtree)
                   (setf (cdr last) new)
                (progn (setf (car subtree) (s (car subtree)))
                       (s-aux subtree (cdr subtree))))))
           (s (subtree)
	      (cond ((satisfies-the-test old subtree) new)
		    ((atom subtree) subtree)
		    (t (s-aux nil subtree)
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
  (declare (system::%java-class-name "jcl.lists.functions.NSubstIf"))
  (labels ((s-aux (last subtree)
             (if (atom subtree)
                 (if (funcall test (apply-key key subtree))
                     (setf (cdr last) new))
               (if (funcall test (apply-key key subtree))
                   (setf (cdr last) new)
                (progn (setf (car subtree) (s (car subtree)))
                       (s-aux subtree (cdr subtree))))))
           (s (subtree)
	      (cond ((funcall predicate (apply-key key subtree)) new)
		    ((atom subtree) subtree)
		    (t (s-aux nil subtree)
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
  (declare (system::%java-class-name "jcl.lists.functions.NSubstIfNot"))
  (labels ((s-aux (last subtree)
             (if (atom subtree)
                 (if (not (funcall test (apply-key key subtree)))
                     (setf (cdr last) new))
               (if (not (funcall test (apply-key key subtree)))
                   (setf (cdr last) new)
                (progn (setf (car subtree) (s (car subtree)))
                       (s-aux subtree (cdr subtree))))))
           (s (subtree)
	      (cond ((not (funcall predicate (apply-key key subtree)) new))
		    ((atom subtree) subtree)
		    (t (s-aux nil subtree)
		       subtree))))
    (s tree)))

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
  (declare (system::%java-class-name "jcl.lists.functions.NSublis"))
  (let (temp)
    (labels ((s-aux (last subtree)
               (if (atom subtree)
                   (if (setq temp (nsublis-macro))
                       (setf (cdr last) (cdr temp)))
                 (if (setq temp (nsublis-macro))
                     (setf (cdr last) (cdr temp))
                  (progn (setf (car subtree) (s (car subtree)))
                         (s-aux subtree (cdr subtree))))))
             (s (subtree)
	        (cond ((setq temp (nsublis-macro)) (cdr temp))
                      ((atom subtree) subtree)
                      (t (s-aux nil subtree)
                         subtree))))
    (s tree))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(defun adjoin (item list &key key (test #'eql testp) (test-not nil notp))
  "Add `item' to `list' unless it is already a member (as determined by
the test function `test'."
  (when (and testp notp)
    (error "test and test-not both supplied"))
  (if (let ((key-val (sys::apply-key key item)))
	(if notp
	    (member key-val list :test-not test-not :key key)
	    (member key-val list :test test :key key)))
      list
      (cons item list)))



(defun tree-equal-test-not (x y test-not)
  (cond ((consp x)
	 (and (consp y)
	      (tree-equal-test-not (car x) (car y) test-not)
	      (tree-equal-test-not (cdr x) (cdr y) test-not)))
	((consp y) nil)
	((not (funcall test-not x y)) t)
	(t ())))

(defun tree-equal-test (x y test)
  (cond	((consp x)
	 (and (consp y)
	      (tree-equal-test (car x) (car y) test)
	      (tree-equal-test (cdr x) (cdr y) test)))
	((consp y) nil)
	((funcall test x y) t)
	(t ())))

(defun tree-equal (x y &key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "test and test-not both supplied"))
  (if test-not
      (tree-equal-test-not x y test-not)
      (tree-equal-test x y test)))



(defun member (item list &key key test test-not)
  (%member item list key test test-not))

(defun member-if (test list &key key)
  (do ((list list (cdr list)))
      ((endp list) nil)
    (if (funcall test (apply-key key (car list)))
	(return list))))

(defun member-if-not (test list &key key)
  (do ((list list (cdr list)))
      ((endp list) ())
    (if (not (funcall test (apply-key key (car list))))
	(return list))))



(defmacro assoc-guts (test-guy)
  `(do ((alist alist (cdr alist)))
       ((endp alist))
     (if (car alist)
	 (if ,test-guy (return (car alist))))))

(defun assoc (item alist &key key test test-not)
  (cond (test
	 (if key
	     (assoc-guts (funcall test item (funcall key (caar alist))))
	     (assoc-guts (funcall test item (caar alist)))))
	(test-not
	 (if key
	     (assoc-guts (not (funcall test-not item
				       (funcall key (caar alist)))))
	     (assoc-guts (not (funcall test-not item (caar alist))))))
	(t
	 (if key
	     (assoc-guts (eql item (funcall key (caar alist))))
	     (assoc-guts (eql item (caar alist)))))))

(defun assoc-if (predicate alist &key key)
  (if key
      (assoc-guts (funcall predicate (funcall key (caar alist))))
      (assoc-guts (funcall predicate (caar alist)))))

(defun assoc-if-not (predicate alist &key key)
  (if key
      (assoc-guts (not (funcall predicate (funcall key (caar alist)))))
      (assoc-guts (not (funcall predicate (caar alist))))))

(defun rassoc (item alist &key key test test-not)
  (cond (test
	 (if key
	     (assoc-guts (funcall test item (funcall key (cdar alist))))
	     (assoc-guts (funcall test item (cdar alist)))))
	(test-not
	 (if key
	     (assoc-guts (not (funcall test-not item
				       (funcall key (cdar alist)))))
	     (assoc-guts (not (funcall test-not item (cdar alist))))))
	(t
	 (if key
	     (assoc-guts (eql item (funcall key (cdar alist))))
	     (assoc-guts (eql item (cdar alist)))))))

(defun rassoc-if (predicate alist &key key)
  (if key
      (assoc-guts (funcall predicate (funcall key (cdar alist))))
      (assoc-guts (funcall predicate (cdar alist)))))

(defun rassoc-if-not (predicate alist &key key)
  (if key
      (assoc-guts (not (funcall predicate (funcall key (cdar alist)))))
      (assoc-guts (not (funcall predicate (cdar alist))))))



(defun map1 (function original-arglists accumulate take-car)
  (let* ((arglists (copy-list original-arglists))
	 (ret-list (list nil))
	 (temp ret-list))
    (do ((res nil)
	 (args '() '()))
        ((dolist (x arglists nil) (if (null x) (return t)))
         (if accumulate
             (cdr ret-list)
             (car original-arglists)))
      (do ((l arglists (cdr l)))
          ((null l))
	(push (if take-car (caar l) (car l)) args)
	(setf (car l) (cdar l)))
      (setq res (apply function (nreverse args)))
      (case accumulate
	(:nconc (setq temp (last (nconc temp res))))
	(:list (rplacd temp (list res))
	       (setq temp (cdr temp)))))))

(defun mapcan (function list &rest more-lists)
  (map1 function (cons list more-lists) :nconc t))

(defun mapl (function list &rest more-lists)
  (map1 function (cons list more-lists) nil nil))

(defun maplist (function list &rest more-lists)
  (map1 function (cons list more-lists) :list nil))

(defun mapcon (function list &rest more-lists)
  (map1 function (cons list more-lists) :nconc nil))



(defmacro with-set-keys (funcall)
  `(cond (notp ,(append funcall '(:key key :test-not test-not)))
	 (t ,(append funcall '(:key key :test test)))))

(defun union (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (require-type list2 'list)
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((res list2))
    (dolist (elt list1)
      (unless (with-set-keys (member (funcall-key key elt) list2))
	(push elt res)))
    res))

(defmacro steve-splice (source destination)
  `(let ((temp ,source))
     (setf ,source (cdr ,source)
	   (cdr temp) ,destination
	   ,destination temp)))

(defun nunion (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((res list2)
	(list1 list1))
    (do ()
        ((endp list1))
      (if (not (with-set-keys (member (funcall-key key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setf list1 (cdr list1))))
    res))

(defun intersection (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((res nil))
    (dolist (elt list1)
      (if (with-set-keys (member (funcall-key key elt) list2))
	  (push elt res)))
    res))

(defun nintersection (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((res nil)
	(list1 list1))
    (do () ((endp list1))
      (if (with-set-keys (member (funcall-key key (car list1)) list2))
	  (steve-splice list1 res)
	  (setq list1 (cdr list1))))
    res))

(defun set-difference (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (if (null list2)
      list1
      (let ((res nil))
	(dolist (elt list1)
	  (if (not (with-set-keys (member (funcall-key key elt) list2)))
	      (push elt res)))
	res)))

(defun nset-difference (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((res nil)
	(list1 list1))
    (do () ((endp list1))
      (if (not (with-set-keys (member (funcall-key key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setq list1 (cdr list1))))
    res))

(defun set-exclusive-or (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((result nil)
        (key (when key (coerce key 'function)))
        (test (coerce test 'function))
        (test-not (if test-not (coerce test-not 'function) #'eql)))
    (dolist (elt list1)
      (unless (with-set-keys (member (funcall-key key elt) list2))
	(setq result (cons elt result))))
    (let ((test (if testp
                    (lambda (x y) (funcall test y x))
                    test))
          (test-not (if notp
                        (lambda (x y) (funcall test-not y x))
                        test-not)))
      (dolist (elt list2)
        (unless (with-set-keys (member (funcall-key key elt) list1))
          (setq result (cons elt result)))))
    result))

;;; Adapted from SBCL.
(defun nset-exclusive-or (list1 list2 &key key (test #'eql testp) (test-not #'eql notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (let ((key (and key (coerce-to-function key)))
        (test (if testp (coerce-to-function test) test))
        (test-not (if notp (coerce-to-function test-not) test-not)))
    ;; The outer loop examines LIST1 while the inner loop examines
    ;; LIST2. If an element is found in LIST2 "equal" to the element
    ;; in LIST1, both are spliced out. When the end of LIST1 is
    ;; reached, what is left of LIST2 is tacked onto what is left of
    ;; LIST1. The splicing operation ensures that the correct
    ;; operation is performed depending on whether splice is at the
    ;; top of the list or not.
    (do ((list1 list1)
         (list2 list2)
         (x list1 (cdr x))
         (splicex ())
         (deleted-y ())
         ;; elements of LIST2, which are "equal" to some processed
         ;; earlier elements of LIST1
         )
        ((endp x)
         (if (null splicex)
             (setq list1 list2)
             (rplacd splicex list2))
         list1)
      (let ((key-val-x (apply-key key (car x)))
            (found-duplicate nil))

        ;; Move all elements from LIST2, which are "equal" to (CAR X),
        ;; to DELETED-Y.
        (do* ((y list2 next-y)
              (next-y (cdr y) (cdr y))
              (splicey ()))
             ((endp y))
          (cond ((let ((key-val-y (apply-key key (car y))))
                   (if notp
                       (not (funcall test-not key-val-x key-val-y))
                       (funcall test key-val-x key-val-y)))
                 (if (null splicey)
                     (setq list2 (cdr y))
                     (rplacd splicey (cdr y)))
                 (setq deleted-y (rplacd y deleted-y))
                 (setq found-duplicate t))
                (t (setq splicey y))))

        (unless found-duplicate
          (setq found-duplicate (with-set-keys (member key-val-x deleted-y))))

        (if found-duplicate
            (if (null splicex)
                (setq list1 (cdr x))
                (rplacd splicex (cdr x)))
            (setq splicex x))))))

;;; Adapted from SBCL.
(defun subsetp (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (require-type list2 'list)
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (let ((key (and key (coerce-to-function key))))
    (dolist (elt list1)
      (unless (with-set-keys (member (funcall-key key elt) list2))
        (return-from subsetp nil)))
    t))



(defmacro satisfies-the-test (item elt)
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key ,elt)))
       (cond (testp (funcall test ,item ,key-tmp))
             (notp (not (funcall test-not ,item ,key-tmp)))
             (t (funcall test ,item ,key-tmp))))))

(defun %subst (new old tree key test testp test-not notp)
  (cond ((satisfies-the-test old tree) new)
        ((atom tree) tree)
        (t (let ((car (%subst new old (car tree) key test testp test-not notp))
                 (cdr (%subst new old (cdr tree) key test testp test-not notp)))
             (if (and (eq car (car tree))
                      (eq cdr (cdr tree)))
                 tree
                 (cons car cdr))))))

(defun subst (new old tree &key key (test #'eql testp) (test-not nil notp))
  (%subst new old tree key test testp test-not notp))

(defun %subst-if (new test tree key)
  (cond ((funcall test (apply-key key tree)) new)
        ((atom tree) tree)
        (t (let ((car (%subst-if new test (car tree) key))
                 (cdr (%subst-if new test (cdr tree) key)))
             (if (and (eq car (car tree))
                      (eq cdr (cdr tree)))
                 tree
                 (cons car cdr))))))

(defun subst-if (new test tree &key key)
  (%subst-if new test tree key))

(defun %subst-if-not (new test tree key)
  (cond ((not (funcall test (apply-key key tree))) new)
        ((atom tree) tree)
        (t (let ((car (%subst-if-not new test (car tree) key))
                 (cdr (%subst-if-not new test (cdr tree) key)))
             (if (and (eq car (car tree))
                      (eq cdr (cdr tree)))
                 tree
                 (cons car cdr))))))

(defun subst-if-not (new test tree &key key)
  (%subst-if-not new test tree key))

(defun nsubst (new old tree &key key (test #'eql testp) (test-not nil notp))
  (labels ((s (subtree)
	      (cond ((satisfies-the-test old subtree) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (cdr subtree)))
                            ((atom subtree)
                             (if (satisfies-the-test old subtree)
                                 (setf (cdr last) new)))
			 (if (satisfies-the-test old subtree)
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
          (s tree)))

(defun nsubst-if (new test tree &key key)
  (labels ((s (subtree)
	      (cond ((funcall test (apply-key key subtree)) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (cdr subtree)))
                            ((atom subtree)
                             (if (funcall test (apply-key key subtree))
                                 (setf (cdr last) new)))
			 (if (funcall test (apply-key key subtree))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
          (s tree)))

(defun nsubst-if-not (new test tree &key key)
  (labels ((s (subtree)
	      (cond ((not (funcall test (apply-key key subtree))) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (cdr subtree)))
                            ((atom subtree)
                             (if (not (funcall test (apply-key key subtree)))
                                 (setf (cdr last) new)))
			 (if (not (funcall test (apply-key key subtree)))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
          (s tree)))



(defun sublis (alist tree &key key (test #'eql) (test-not nil notp))
  (labels ((s (subtree)
              (let* ((key-val (sys::apply-key key subtree))
                     (assoc (if notp
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

(defmacro nsublis-macro ()
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (sys::apply-key key subtree)))
       (if notp
           (assoc ,key-tmp alist :test-not test-not)
           (assoc ,key-tmp alist :test test)))))

(defun nsublis (alist tree &key key (test #'eql) (test-not nil notp))
  (let (temp)
    (labels ((s (subtree)
		(cond ((setq temp (nsublis-macro))
		       (cdr temp))
		      ((atom subtree) subtree)
		      (t (do* ((last nil subtree)
			       (subtree subtree (cdr subtree)))
                              ((atom subtree)
                               (if (setq temp (nsublis-macro))
                                   (setf (cdr last) (cdr temp))))
			   (if (setq temp (nsublis-macro))
			       (return (setf (cdr last) (cdr temp)))
			       (setf (car subtree) (s (car subtree)))))
			 subtree))))
            (s tree))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun adjoin (item list &key key (test #'eql) (test-not nil notp))
  "Add item to list unless it is already a member"
  (declare (inline member))
  (if (let ((key-val (apply-key key item)))
	(if notp
	    (member key-val list :test-not test-not :key key)
	    (member key-val list :test test :key key)))
      list
      (cons item list)))



(defun tree-equal-test-not (x y test-not)
  (cond ((consp x)
	 (and (consp y)
	      (tree-equal-test-not (car x) (car y) test-not)
	      (tree-equal-test-not (cdr x) (cdr y) test-not)))
	((consp y) nil)
	((not (funcall test-not x y)) t)
	(t ())))

(defun tree-equal-test (x y test)
  (cond	((consp x)
	 (and (consp y)
	      (tree-equal-test (car x) (car y) test)
	      (tree-equal-test (cdr x) (cdr y) test)))
	((consp y) nil)
	((funcall test x y) t)
	(t ())))

(defun tree-equal (x y &key (test #'eql) test-not)
  "Returns T if X and Y are isomorphic trees with identical leaves."
  (if test-not
      (tree-equal-test-not x y test-not)
      (tree-equal-test x y test)))



(defun member (item list &key key (test #'eql testp) (test-not nil notp))
  "Returns tail of list beginning with first element satisfying EQLity,
   :test, or :test-not with a given item."
  (do ((list list (cdr list)))
      ((null list) nil)
    (let ((car (car list)))
      (if (satisfies-the-test item car)
	  (return list)))))

(defun member-if (test list &key key)
  "Returns tail of list beginning with first element satisfying test(element)"
  (do ((list list (Cdr list)))
      ((endp list) nil)
    (if (funcall test (apply-key key (car list)))
	(return list))))

(defun member-if-not (test list &key key)
  "Returns tail of list beginning with first element not satisfying test(el)"
  (do ((list list (cdr list)))
      ((endp list) ())
    (if (not (funcall test (apply-key key (car list))))
	(return list))))



;;; In run-time environment, since these guys can be inline expanded.
(defmacro assoc-guts (test-guy)
  `(do ((alist alist (cdr alist)))
       ((endp alist))
     (declare (optimize (inhibit-warnings 3)))
     (if (car alist)
	 (if ,test-guy (return (car alist))))))

(defun assoc (item alist &key key test test-not)
  "Returns the cons in alist whose car is equal (by a given test or EQL) to
   the Item."
  (cond (test
	 (if key
	     (assoc-guts (funcall test item (funcall key (caar alist))))
	     (assoc-guts (funcall test item (caar alist)))))
	(test-not
	 (if key
	     (assoc-guts (not (funcall test-not item
				       (funcall key (caar alist)))))
	     (assoc-guts (not (funcall test-not item (caar alist))))))
	(t
	 (if key
	     (assoc-guts (eql item (funcall key (caar alist))))
	     (assoc-guts (eql item (caar alist)))))))

(defun assoc-if (predicate alist &key key)
  "Returns the first cons in alist whose car satisfies the Predicate.  If
   key is supplied, apply it to the car of each cons before testing."
  (if key
      (assoc-guts (funcall predicate (funcall key (caar alist))))
      (assoc-guts (funcall predicate (caar alist)))))

(defun assoc-if-not (predicate alist &key key)
  "Returns the first cons in alist whose car does not satisfiy the Predicate.
  If key is supplied, apply it to the car of each cons before testing."
  (if key
      (assoc-guts (not (funcall predicate (funcall key (caar alist)))))
      (assoc-guts (not (funcall predicate (caar alist))))))

(defun rassoc (item alist &key key test test-not)
  (declare (list alist))
  "Returns the cons in alist whose cdr is equal (by a given test or EQL) to
   the Item."
  (cond (test
	 (if key
	     (assoc-guts (funcall test item (funcall key (cdar alist))))
	     (assoc-guts (funcall test item (cdar alist)))))
	(test-not
	 (if key
	     (assoc-guts (not (funcall test-not item
				       (funcall key (cdar alist)))))
	     (assoc-guts (not (funcall test-not item (cdar alist))))))
	(t
	 (if key
	     (assoc-guts (eql item (funcall key (cdar alist))))
	     (assoc-guts (eql item (cdar alist)))))))

(defun rassoc-if (predicate alist &key key)
  "Returns the first cons in alist whose cdr satisfies the Predicate.  If key
  is supplied, apply it to the cdr of each cons before testing."
  (if key
      (assoc-guts (funcall predicate (funcall key (cdar alist))))
      (assoc-guts (funcall predicate (cdar alist)))))

(defun rassoc-if-not (predicate alist &key key)
  "Returns the first cons in alist whose cdr does not satisfy the Predicate.
  If key is supplied, apply it to the cdr of each cons before testing."
  (if key
      (assoc-guts (not (funcall predicate (funcall key (cdar alist)))))
      (assoc-guts (not (funcall predicate (cdar alist))))))



(defun map1 (function original-arglists accumulate take-car)
  "This function is called by mapc, mapcar, mapcan, mapl, maplist, and mapcon.
  It Maps function over the arglists in the appropriate way. It is done when any
  of the arglists runs out.  Until then, it CDRs down the arglists calling the
  function and accumulating results as desired."

  (let* ((arglists (copy-list original-arglists))
	 (ret-list (list nil)) 
	 (temp ret-list))
    (do ((res nil)
	 (args '() '()))
	((dolist (x arglists nil) (if (null x) (return t)))
	 (if accumulate
	     (cdr ret-list)
	     (car original-arglists)))
      (do ((l arglists (cdr l)))
	  ((null l))
	(push (if take-car (caar l) (car l)) args)
	(setf (car l) (cdar l)))
      (setq res (apply function (nreverse args)))
      (case accumulate
	(:nconc (setq temp (last (nconc temp res))))
	(:list (rplacd temp (list res))
	       (setq temp (cdr temp)))))))

(defun mapc (function list &rest more-lists)
  "Applies Function to successive elements of each List, and returns
  its second argument."
  (map1 function (cons list more-lists) nil t))

(defun mapcar (function list &rest more-lists)
  "Applies Function to successive elements of each List, and returns a
  list of results."
  (map1 function (cons list more-lists) :list t))

(defun mapcan (function list &rest more-lists)
  "Applies Function to successive elements of each List, and returns
  NCONC of results."
  (map1 function (cons list more-lists) :nconc t))

(defun mapl (function list &rest more-lists)
  "Applies Function to successive CDRs of each List, and returns ()."
  (map1 function (cons list more-lists) nil nil))

(defun maplist (function list &rest more-lists)
  "Applies Function to successive CDRs of each List, and returns list
  of results."
  (map1 function (cons list more-lists) :list nil))

(defun mapcon (function list &rest more-lists)
  "Applies Function to successive CDRs of each List, and returns NCONC
  of results."
  (map1 function (cons list more-lists) :nconc nil))



;;; UNION -- Public.
;;;
;;; This function assumes list2 is the result, adding to it from list1 as
;;; necessary.  List2 must initialize the result value, so the call to MEMBER
;;; will apply the test to the elements from list1 and list2 in the correct
;;; order.
;;;
(defun union (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Returns the union of list1 and list2."
  (declare (inline member))
  (when (and testp notp) (error (intl:gettext "Test and test-not both supplied.")))
  (let ((res list2))
    (dolist (elt list1)
      (unless (with-set-keys (member (apply-key key elt) list2))
	(push elt res)))
    res))

;;; Destination and source are setf-able and many-evaluable.  Sets the source
;;; to the cdr, and "conses" the 1st elt of source to destination.
;;;
(defmacro steve-splice (source destination)
  `(let ((temp ,source))
     (setf ,source (cdr ,source)
	   (cdr temp) ,destination
	   ,destination temp)))

(defun nunion (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Destructively returns the union list1 and list2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res list2)
	(list1 list1))
    (do ()
	((endp list1))
      (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setf list1 (cdr list1))))
    res))

(defun intersection (list1 list2 &key key
			   (test #'eql testp) (test-not nil notp))
  "Returns the intersection of list1 and list2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil))
    (dolist (elt list1)
      (if (with-set-keys (member (apply-key key elt) list2))
	  (push elt res)))
    res))

(defun nintersection (list1 list2 &key key
			    (test #'eql testp) (test-not nil notp))
  "Destructively returns the intersection of list1 and list2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil)
	(list1 list1))
    (do () ((endp list1))
      (if (with-set-keys (member (apply-key key (car list1)) list2))
	  (steve-splice list1 res)
	  (setq list1 (Cdr list1))))
    res))

(defun set-difference (list1 list2 &key key
			     (test #'eql testp) (test-not nil notp))
  "Returns the elements of list1 which are not in list2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (if (null list2)
      list1
      (let ((res nil))
	(dolist (elt list1)
	  (if (not (with-set-keys (member (apply-key key elt) list2)))
	      (push elt res)))
	res)))

(defun nset-difference (list1 list2 &key key
			      (test #'eql testp) (test-not nil notp))
  "Destructively returns the elements of list1 which are not in list2."
  (declare (inline member))
  (if (and testp notp)
      (error "Test and test-not both supplied."))
  (let ((res nil)
	(list1 list1))
    (do () ((endp list1))
      (if (not (with-set-keys (member (apply-key key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setq list1 (cdr list1))))
    res))

(defun set-exclusive-or (list1 list2 &key key
                         (test #'eql testp) (test-not nil notp))
  "Return new list of elements appearing exactly once in LIST1 and LIST2."
  (declare (inline member))
  (let ((result nil)
        (key (when key (coerce key 'function)))
        (test (coerce test 'function))
        (test-not (if test-not (coerce test-not 'function) #'eql)))
    (declare (type (or function null) key)
             (type function test test-not))
    (dolist (elt list1)
      (unless (with-set-keys (member (apply-key key elt) list2))
	(setq result (cons elt result))))
    (let ((test (if testp
                    (lambda (x y) (funcall test y x))
                    test))
          (test-not (if notp
                        (lambda (x y) (funcall test-not y x))
                        test-not)))
      (dolist (elt list2)
        (unless (with-set-keys (member (apply-key key elt) list1))
          (setq result (cons elt result)))))
    result))

;;; The outer loop examines list1 while the inner loop examines list2. If an
;;; element is found in list2 "equal" to the element in list1, both are
;;; spliced out. When the end of list1 is reached, what is left of list2 is
;;; tacked onto what is left of list1.  The splicing operation ensures that
;;; the correct operation is performed depending on whether splice is at the
;;; top of the list or not.
(defun nset-exclusive-or (list1 list2
				&key key (test #'eql testp) (test-not #'eql notp))
  "Destructively return a list with elements which appear but once in LIST1
   and LIST2."
  (when (and testp notp)
    (error ":TEST and :TEST-NOT were both supplied."))
  ;; The outer loop examines LIST1 while the inner loop examines
  ;; LIST2. If an element is found in LIST2 "equal" to the element
  ;; in LIST1, both are spliced out. When the end of LIST1 is
  ;; reached, what is left of LIST2 is tacked onto what is left of
  ;; LIST1. The splicing operation ensures that the correct
  ;; operation is performed depending on whether splice is at the
  ;; top of the list or not.
  (do ((list1 list1)
       (list2 list2)
       (x list1 (cdr x))
       (splicex ())
       ;; elements of LIST2, which are "equal" to some processed
       ;; earlier elements of LIST1
       (deleted-y ()))
      ((endp x)
       (if (null splicex)
	   (setq list1 list2)
	   (rplacd splicex list2))
       list1)
    (let ((key-val-x (apply-key key (car x)))
	  (found-duplicate nil))

      ;; Move all elements from LIST2, which are "equal" to (CAR X),
      ;; to DELETED-Y.
      (do* ((y list2 next-y)
	    (next-y (cdr y) (cdr y))
	    (splicey ()))
	   ((endp y))
	(cond ((let ((key-val-y (apply-key key (car y))))
		 (if notp
		     (not (funcall test-not key-val-x key-val-y))
		     (funcall test key-val-x key-val-y)))
	       (if (null splicey)
		   (setq list2 (cdr y))
		   (rplacd splicey (cdr y)))
	       (setq deleted-y (rplacd y deleted-y))
	       (setq found-duplicate t))
	      (t (setq splicey y))))

      (unless found-duplicate
	(setq found-duplicate (with-set-keys (member key-val-x deleted-y))))

      (if found-duplicate
	  (if (null splicex)
	      (setq list1 (cdr x))
	      (rplacd splicex (cdr x)))
	  (setq splicex x)))))

(defun subsetp (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  "Returns T if every element in list1 is also in list2."
  (declare (inline member))
  (dolist (elt list1)
    (unless (with-set-keys (member (apply-key key elt) list2))
      (return-from subsetp nil)))
  T)



;;; APPLY-KEY saves us a function call sometimes.
;;;    This is not in and (eval-when (compile eval) ...
;;;    because this is used in seq.lisp and sort.lisp.
;;;
(defmacro apply-key (key element)
  `(if ,key
       (funcall ,key ,element)
       ,element))

;;;; Macros for (&key (key #'identity) (test #'eql testp) (test-not nil notp)).
;;; Use these with the following keyword args:
;;;
(defmacro with-set-keys (funcall)
  `(cond ((and testp notp) (error "Test and test-not both supplied."))
	 (notp ,(append funcall '(:key key :test-not test-not)))
	 (t ,(append funcall '(:key key :test test)))))

(defmacro satisfies-the-test (item elt)
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key ,elt)))
      (cond (testp (funcall test ,item ,key-tmp))
	    (notp (not (funcall test-not ,item ,key-tmp)))
	    (t (funcall test ,item ,key-tmp))))))

(defun subst (new old tree &key key (test #'eql testp) (test-not nil notp))
  "Substitutes new for subtrees matching old."
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

(defun subst-if (new test tree &key key)
  "Substitutes new for subtrees for which test is true."
  (labels ((s (subtree)
	      (cond ((funcall test (apply-key key subtree)) new)
		    ((atom subtree) subtree)
		    (t (let ((car (s (car subtree)))
			     (cdr (s (cdr subtree))))
			 (if (and (eq car (car subtree))
				  (eq cdr (cdr subtree)))
			     subtree
			     (cons car cdr)))))))
    (s tree)))

(defun subst-if-not (new test tree &key key)
  "Substitutes new for subtrees for which test is false."
  (labels ((s (subtree)
	      (cond ((not (funcall test (apply-key key subtree))) new)
		    ((atom subtree) subtree)
		    (t (let ((car (s (car subtree)))
			     (cdr (s (cdr subtree))))
			 (if (and (eq car (car subtree))
				  (eq cdr (cdr subtree)))
			     subtree
			     (cons car cdr)))))))
    (s tree)))

(defun nsubst (new old tree &key key (test #'eql testp) (test-not nil notp))
  "Substitutes new for subtrees matching old."
  (labels ((s (subtree)
	      (cond ((satisfies-the-test old subtree) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (satisfies-the-test old subtree)
				 (setf (cdr last) new)))
			 (if (satisfies-the-test old subtree)
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))

(defun nsubst-if (new test tree &key key)
  "Substitutes new for subtrees of tree for which test is true."
  (labels ((s (subtree)
	      (cond ((funcall test (apply-key key subtree)) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (funcall test (apply-key key subtree))
				 (setf (cdr last) new)))
			 (if (funcall test (apply-key key subtree))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))

(defun nsubst-if-not (new test tree &key key)
  "Substitutes new for subtrees of tree for which test is false."
  (labels ((s (subtree)
	      (cond ((not (funcall test (apply-key key subtree))) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (Cdr subtree)))
			    ((atom subtree)
			     (if (not (funcall test (apply-key key subtree)))
				 (setf (cdr last) new)))
			 (if (not (funcall test (apply-key key subtree)))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
    (s tree)))



(defun sublis (alist tree &key key (test #'eql) (test-not nil notp))
  "Substitutes from alist into tree nondestructively."
  (declare (inline assoc))
  (labels ((s (subtree)
	     (let* ((key-val (apply-key key subtree))
		    (assoc (if notp
			       (assoc key-val alist :test-not test-not)
			       (assoc key-val alist :test test))))
	       (cond (assoc (cdr assoc))
		     ((atom subtree) subtree)
		     (t (let ((car (s (car subtree)))
			      (cdr (s (cdr subtree))))
			  (if (and (eq car (car subtreE))
				   (eq cdr (cdr subtree)))
			      subtree
			      (cons car cdr))))))))
    (s tree)))

;;; In run-time env, since can be referenced in line expansions.
(defmacro nsublis-macro ()
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key subtree)))
      (if notp
	  (assoc ,key-tmp alist :test-not test-not)
	  (assoc ,key-tmp alist :test test)))))

(defun nsublis (alist tree &key key (test #'eql) (test-not nil notp))
  "Substitutes new for subtrees matching old."
  (declare (inline assoc))
  (let (temp)
    (labels ((s (subtree)
		(cond ((Setq temp (nsublis-macro))
		       (cdr temp))
		      ((atom subtree) subtree)
		      (t (do* ((last nil subtree)
			       (subtree subtree (Cdr subtree)))
			      ((atom subtree)
			       (if (setq temp (nsublis-macro))
				   (setf (cdr last) (cdr temp))))
			   (if (setq temp (nsublis-macro))
			       (return (setf (Cdr last) (Cdr temp)))
			       (setf (car subtree) (s (car subtree)))))
			 subtree))))
      (s tree))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Adapted from SBCL.
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

;; Adapted from SBCL.
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

;; Adapted from SBCL.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;

(export '(defmacro defun)
        "COMMON-LISP")
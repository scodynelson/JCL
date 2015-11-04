

(defun parse-body (body &optional (doc-string-allowed t))
  (let ((decls nil)
        (doc nil)
        (form (car body)))
    (tagbody
      top
      (cond ((null body)
             (values body decls doc))
            ((and (stringp form) (cdr body))
             (if doc-string-allowed
                 (setq doc form doc-string-allowed nil body (cdr body) form (car body))
               (return (values body decls doc))))
            ((not (and (consp form) (symbolp (car form))))
		     (return (values body decls doc)))
		    ((eq (car form) 'declare)
		     (setq decls (cons form decls) body (cdr body) form (car body)))
		    (t
		     (return (values body decls doc)))))))

;CL4Java
(defmacro dolist ((var list-form &optional result-form) &body body)
  (declare (system::%java-class-name "jcl.compiler.functions.Dolist"))
  (multiple-value-bind (forms decls)
                       (parse-body body nil)
    (let ((list (gensym "LIST-"))
          (top (gensym "TOP-")))
      `(block nil
         (let ((,list ,list-form)
               (,var nil))
           ,@decls
           (tagbody
             ,top
             (when ,list
               (setq ,var (car ,list))
               (setq ,list (cdr ,list))
               (tagbody ,@forms)
               (go ,top))
             ,`,result-form))))))


;CL4Java
;;; Make sure we iterate the given number of times, independent of
;;; what the body might do to the index variable.  We do this by
;;; repeatedly binding the var in the body and also in the result
;;; form.  We also spuriously reference the var in case the body or
;;; result form don't reference the var either.  (Mostly modeled on
;;; the dolist macro below.)
(defmacro dotimes ((var count-form &optional result-form) &body body)
  (declare (system::%java-class-name "lisp.common.function.Dotimes"))
  (let ((cnt (gensym "COUNT-"))
        (top (gensym "TAG-"))
        (decls nil))
    (tagbody
      label1
      (when (eq (car (car body)) 'declare)
        (progn
          (setq decls (cons (car body) decls))
          (setq body (cdr body))
          (go label1))))
    `(block nil
       (let ((,var 0)
             (,cnt ,count-form))
         ,@decls
         (tagbody
           ,top
           (if (< ,var ,cnt)
               (tagbody
                 ,@body
                 (setq ,var (1+ ,var))
                 (go ,top))
             (setq ,var nil)))
         ,result-form))))

;CMUCL
(defmacro dotimes ((var count &optional (result nil)) &body body)
  (let ((count-var (gensym "CTR-")))
    (multiple-value-bind (forms decls)
						 (parse-body body nil nil)
      (multiple-value-bind (var-decls ctr-decls)
	                       (dotimes-extract-var-decls var count-var count decls)
		(cond ((numberp count)
		       `(do ((,count-var 0 (1+ ,count-var)))
				    ((>= ,count-var ,count)
				     (let ((,var ,count-var))
				       ,@var-decls
				       ,var
				       ,result))
				  ,@ctr-decls
				  (let ((,var ,count-var))
				    ,@decls
				    ,var
				    (tagbody
				      ,@forms))))
		      (t (let ((v1 (gensym)))
				   `(do ((,count-var 0 (1+ ,count-var))
					     (,v1 ,count))
						((>= ,count-var ,v1)
						 (let ((,var ,count-var))
						   ,@var-decls
						   ,var
						   ,result))
				      ,@ctr-decls
				      (let ((,var ,count-var))
						,@decls
						,var
						(tagbody
						  ,@forms))))))))))

;ABCL
(defmacro dotimes ((var count &optional (result nil)) &body body)
  (multiple-value-bind (forms decls)
                       (parse-body body nil)
    (let ((index (gensym "INDEX-"))
          (top (gensym "TOP-")))
      (if (numberp count)
          `(block nil
             (let ((,var 0)
                   (,index 0))
               ,@decls
               (when (> ,count 0)
                 (tagbody
                  ,top
                  ,@forms
                  (setq ,index (1+ ,index))
                  (setq ,var ,index)
                  (when (< ,index ,count)
                    (go ,top))))
               (progn ,result)))
        (let ((limit (gensym "LIMIT-")))
          ;; Annotations for the compiler.
          (setf (get limit 'dotimes-limit-variable-p) t)
          (setf (get index 'dotimes-index-variable-name) index)
          (setf (get index 'dotimes-index-variable-p) t)
          (setf (get limit 'dotimes-limit-variable-name) limit)
          `(block nil
             (let ((,var 0)
                   (,limit ,count)
                   (,index 0))
               ,@decls
               (when (> ,limit 0)
                 (tagbody
                  ,top
                  ,@forms
                  (setq ,index (1+ ,index))
                  (setq ,var ,index)
                  (when (< ,index ,limit)
                    (go ,top))))
               (progn ,result))))))))




#| DO AND DO* |#

(defun fill-out-var (var)
  (unless (listp var)
    (setq var (list var)))
  `(,(first var) ,(second var) ,@(when (third var) (list (third var)))))

(defun fill-out-vars (vars)
  (when vars
    (cons (fill-out-var (car vars)) (fill-out-vars (rest vars)))))

(defun initialize-vars (vars)
  (when vars
    (let ((first-var (first vars)))
      (cons `(,(first first-var) ,(second first-var)) (initialize-vars (rest vars))))))

(defun update-vars (vars)
  (when vars
    (if (cddar vars)
        (cons `(,(caar vars) ,(caddar vars)) (update-vars (rest vars)))
      (update-vars (rest vars)))))

(defun end-test-form (test-and-results)
  (first test-and-results))

(defun result-forms (test-and-results)
  (rest test-and-results))

(defun count-decl-forms (the-listof-stmts &optional (count 0))
  (if (eq (caar the-listof-stmts) 'declare)
      (count-decl-forms (rest the-listof-stmts) (1+ count))
    count))

(defun parse-decls-and-statements (decls-and-statements)
  (let ((count-of-decls (count-decl-forms decls-and-statements)))
    `(,(copy-first-n decls-and-statements count-of-decls) ,(nthcdr count-of-decls decls-and-statements))))

(defun count-decl-forms (the-listof-stmts &optional (count 0))
  (if (eq (caar the-listof-stmts) 'declare)
      (count-decl-forms (rest the-listof-stmts) (1+ count))
    count))

(defun copy-first-n (list count)
 (when (plusp count)
   (cons (first list) (copy-first-n (rest list) (1- count)))))

(defun flatten-pairs (pairs)
  (when pairs
    (cons (caar pairs) (cons (cadar pairs) (flatten-pairs (rest pairs))))))

(defmacro do (vars (end-test-form &rest result-forms) &rest decls-and-statements)
  (declare (system::%java-class-name "lisp.common.function.Do"))
  (let* ((full-vars (fill-out-vars vars))
         (init-vars (initialize-vars full-vars))
         (update-vars (update-vars full-vars))
         (decls-stmts (parse-decls-and-statements decls-and-statements))
         (decls (first decls-stmts))
         (stmts (second decls-stmts))
         (top-label (gensym "TopLabel-"))
         (end-label (gensym "EndLabel-")))
    `(block nil
       (let ,init-vars
         (declare ,@decls)
         (tagbody
           ,top-label
           (if ,end-test-form
               (go ,end-label)
             (progn
               ,@stmts
               (psetq ,@(flatten-pairs update-vars))
               (go ,top-label)))
           ,end-label)
           ,@result-forms))))

(defmacro do* (vars (end-test-form &rest result-forms) &rest decls-and-statements)
  (declare (system::%java-class-name "lisp.common.function.DoStar"))
  (let* ((full-vars (fill-out-vars vars))
         (init-vars (initialize-vars full-vars))
         (update-vars (update-vars full-vars))
         (decls-stmts (parse-decls-and-statements decls-and-statements))
         (decls (first decls-stmts))
         (stmts (second decls-stmts))
         (top-label (gensym "TopLabel-"))
         (end-label (gensym "EndLabel-")))
    `(block nil
       (let* ,init-vars
         (declare ,@decls)
         (tagbody
           ,top-label
           (if ,end-test-form
               (go ,end-label)
             (progn
               ,@stmts
               (setq ,@(flatten-pairs update-vars))
               (go ,top-label)))
           ,end-label)
           ,@result-forms))))


;ABCL
(defun do-do-body (varlist endlist decls-and-code bind step name block)
  (let* ((inits ())
	 (steps ())
	 (L1 (gensym))
	 (L2 (gensym)))
    ;; Check for illegal old-style do.
    (when (or (not (listp varlist)) (atom endlist))
      (error "Ill-formed ~S -- possibly illegal old style DO?" name))
    ;; Parse the varlist to get inits and steps.
    (dolist (v varlist)
      (cond ((symbolp v) (push v inits))
	    ((listp v)
	     (unless (symbolp (first v))
	       (error "~S step variable is not a symbol: ~S" name (first v)))
	     (case (length v)
	       (1 (push (first v) inits))
	       (2 (push v inits))
	       (3 (push (list (first v) (second v)) inits)
		  (setq steps (list* (third v) (first v) steps)))
	       (t (error "~S is an illegal form for a ~S varlist." v name))))
	    (t (error "~S is an illegal form for a ~S varlist." v name))))
    ;; Construct the new form.
    (multiple-value-bind (code decls) (parse-body decls-and-code nil)
      `(block ,block
         (,bind ,(nreverse inits)
          ,@decls
          (tagbody
           (go ,L2)
           ,L1
           ,@code
           (,step ,@(nreverse steps))
           ,L2
           (unless ,(car endlist) (go ,L1))
           (return-from ,block (progn ,@(cdr endlist)))))))))

(defmacro do (varlist endlist &rest body)
  (do-do-body varlist endlist body 'let 'psetq 'do nil))

(defmacro do* (varlist endlist &rest body)
  (do-do-body varlist endlist body 'let* 'setq 'do* nil))
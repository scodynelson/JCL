;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
  (require "macros")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

(defun make-hash-table (&key (test 'eql) (size 50) (rehash-size 1) (rehash-threshold 0.75))
  "Creates and returns a new hash table."
  (declare (system::%java-class-name "jcl.hashtables.functions.MakeHashTable")
           (ignore rehash-size))
  (unless (and (integerp size) (>= size 0))
    (error "Expected non-negative integer for table size."))
  (ext:jinvoke-static
    (ext:jmethod "toLispHashTable" (ext:jclass "jcl.lang.HashTableStruct")
                 (ext:jclass "jcl.lang.LispStruct")
                 (ext:jclass "jcl.lang.FixnumStruct")
                 (ext:jclass "jcl.lang.FloatStruct"))
    test size rehash-threshold))

;;;;;;;;;;;;;;;;;;;;;;

(defun hash-table-test (hash-table)
  "Returns the test used for comparing keys in hash-table."
  (declare (system::%java-class-name "jcl.hashtables.functions.HashTableTest"))
  ($test hash-table))

(defun hash-table-size (hash-table)
  "Returns the current size of hash-table."
  (declare (system::%java-class-name "jcl.hashtables.functions.HashTableSize"))
  ($size hash-table))

(defun hash-table-rehash-size (hash-table)
  "Returns the current rehash size of hash-table."
  (declare (system::%java-class-name "jcl.hashtables.functions.HashTableRehashSize"))
  ($rehashSize hash-table))

(defun hash-table-rehash-threshold (hash-table)
  "Returns the current rehash threshold of hash-table."
  (declare (system::%java-class-name "jcl.hashtables.functions.HashTableRehashThreshold"))
  ($rehashThreshold hash-table))

(defun hash-table-count (hash-table)
  "Returns the number of entries in the hash-table."
  (declare (system::%java-class-name "jcl.hashtables.functions.HashTableCount"))
  ($count hash-table))

;;;;;;;;;;;;;;;;;;;;;;

(defun gethash (key hash-table &optional default)
  "Retrieve the value in the hash-table whose key is the same as the key under the hash-table's equivalence test.
  If there is no such entry, the result is the default."
  (declare (system::%java-class-name "jcl.hashtables.functions.GetHash"))
  ($getHash hash-table key default))

(defun (setf gethash) (new-value key hash-table &optional default)
  "Set the value (either by addition or modification) in the hash-table whose key is the same as the key under the
  hash-table's equivalence test. The optional default value is evaluated, but ignored."
  (declare (system::%java-class-name "jcl.hashtables.functions.SetfGetHash")
           (ignore default))
  ($putHash hash-table key new-value))

(defun remhash (key hash-table)
  "Removes the entry for key in hash-table, if any. Returns true if there was such an entry, or false otherwise."
  (declare (system::%java-class-name "jcl.hashtables.functions.RemHash"))
  ($remHash hash-table key))

(defun clrhash (hash-table)
  "Removes all entries from hash-table, and then returns that empty hash table."
  (declare (system::%java-class-name "jcl.hashtables.functions.ClrHash"))
  ($clrHash hash-table))

(defun sxhash (object)
  "Returns a hash code for object."
  (declare (system::%java-class-name "jcl.hashtables.functions.SxHash"))
  (ext:jinvoke-static
    (ext:jmethod "sxHash" (ext:jclass "jcl.lang.HashTableStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    object))

(defun maphash (function hash-table)
  "Iterates over all entries in the hash-table. For each entry, the function is called with two arguments--the key and
  the value of that entry."
  (declare (system::%java-class-name "jcl.hashtables.functions.MapHash"))
  ($mapHash hash-table function))

;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  "Has within the lexical scope of the body, a name which is defined via macrolet such that successive invocations
   of (name) return the items, one by one, from the hash table that is obtained by evaluating hash-table only once.
   After all entries have been returned by successive invocations of (name), then only one value is returned, namely nil."
  (declare (system::%java-class-name "jcl.hashtables.functions.WithHashTableIterator"))
  (let ((the-function (gensym "WITH-HASH-TABLE-ITERATOR-")))
    `(let ((,the-function (let ((entries ($entries ,hash-table)))
                            (labels ((,name ()
                                       (let ((entry (car entries)))
                                         (setq entries (cdr entries))
                                         (if entry
                                             (values t (car entry) (cdr entry))
                                           nil))))
                              #',name))))
       (macrolet ((,name () '(funcall ,the-function)))
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;

(export '(make-hash-table
          hash-table-test hash-table-size hash-table-rehash-size hash-table-rehash-threshold hash-table-count
          gethash remhash clrhash sxhash maphash
          with-hash-table-iterator)
        "COMMON-LISP")

(provide "hashtables")
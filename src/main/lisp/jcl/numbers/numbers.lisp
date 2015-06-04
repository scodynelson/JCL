;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:load-toplevel :execute)
(export '()
        (find-package "COMMON-LISP"))
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

(defun 1+ (number)
  (+ number 1))

(defun 1- (number)
  (- number 1))

;;;;;;;;;;;;;;;;;;;;;;

(defun byte (size position)
  (cons size position))

(defun byte-size (bytespec)
  (car bytespec))

(defun byte-position (bytespec)
  (cdr bytespec))

(defun ldb (bytespec integer)
  (logand (ash integer (- (byte-position bytespec)))
          (1- (ash 1 (byte-size bytespec)))))

(defun ldb-test (bytespec integer)
  (not (zerop (ldb bytespec integer))))

(defun dpb (newbyte bytespec integer)
  (let* ((size (byte-size bytespec))
         (position (byte-position bytespec))
         (mask (1- (ash 1 size))))
    (logior (logand integer (lognot (ash mask position)))
	        (ash (logand newbyte mask) position))))

;;;;;;;;;;;;;;;;;;;;;;

(defun phase (number)
  "Returns the angle part of the polar representation of a complex number.
   For complex numbers, this is (atan (imagpart number) (realpart number)).
   For non-complex positive numbers, this is 0.  For non-complex negative
   numbers this is PI."
  (etypecase number
             (rational
              (if (minusp number)
                  (coerce pi 'float)
                0.0))
             (float
              (if (minusp (float-sign number))
                  (coerce pi 'float)
                0.0))
             (complex
              (if (zerop (realpart number))
                  (coerce (* (/ pi 2) (signum (imagpart number))) float)
                (atan (imagpart number) (realpart number))))))

;;;;;;;;;;;;;;;;;;;;;;

(defun boole (op n1 n2)
  (unless (and (integerp n1) (integerp n2))
    (error 'type-error
           :datum (if (integerp n1) n2 n1)
           :expected-type 'integer))
  (case op
    (#.boole-clr 0)
    (#.boole-set -1)
    (#.boole-1 n1)
    (#.boole-2 n2)
    (#.boole-c1 (lognot n1))
    (#.boole-c2 (lognot n2))
    (#.boole-and (logand n1 n2))
    (#.boole-ior (logior n1 n2))
    (#.boole-xor (logxor n1 n2))
    (#.boole-eqv (logeqv n1 n2))
    (#.boole-nand (lognand n1 n2))
    (#.boole-nor (lognor n1 n2))
    (#.boole-andc1 (logandc1 n1 n2))
    (#.boole-andc2 (logandc2 n1 n2))
    (#.boole-orc1 (logorc1 n1 n2))
    (#.boole-orc2 (logorc2 n1 n2))
    (t
     (error 'type-error
            :datum op
            :expected-type (list 'integer #.boole-clr #.boole-orc2)))))

;;;;;;;;;;;;;;;;;;;;;;

(defun parse-integer-error (string)
  (error 'parse-error "not an integer string: ~S" string))

(defun parse-integer (string &key (start 0)
								  end
                                  (radix 10)
                                  junk-allowed)
  (when (null end)
    (setq end (length string)))
  (let ((index (do ((i start (1+ i)))
                   ((= i end)
                    (if junk-allowed
                        (return-from parse-integer (values nil end))
                      (parse-integer-error string)))
                 (unless (whitespacep (char string i))
                   (return i))))
        (minusp nil)
        (found-digit nil)
        (result 0))
    (let ((char (char string index)))
      (cond ((char= char #\-)
             (setq minusp t)
             (setq index (1+ index)))
            ((char= char #\+)
             (setq index (1+ index)))))
    (loop
      (when (= index end)
        (return nil))
      (let* ((char (char string index))
             (weight (digit-char-p char radix)))
        (cond (weight
               (setq result (+ weight (* result radix))
                            found-digit t))
              (junk-allowed
               (return nil))
              ((whitespacep char)
               (do ()
                   ((= (setq index (1+ index)) end))
                 (unless (whitespacep (char string index))
                   (parse-integer-error string)))
               (return nil))
              (t
               (parse-integer-error string))))
      (setq index (1+ index)))
    (values
     (if found-digit
         (if minusp (- result) result)
       (if junk-allowed
           nil
         (parse-integer-error string)))
     index)))

;;;;;;;;;;;;;;;;;;;;;;

(defun upgraded-complex-part-type (typespec &optional environment)
  (declare (ignore environment))
  (if (subtypep typespec 'REAL)
      typespec
    (error 'simple-error
           :format-control "The type ~S is not a subtype of ~S."
           :format-arguments (list typespec 'REAL))))

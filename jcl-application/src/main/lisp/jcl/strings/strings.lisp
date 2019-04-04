;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
#|
(eval-when (:load-toplevel :execute)
(export '()
        (find-package "COMMON-LISP"))
) ;eval-when
|#
(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: stringp, simple-string-p

;;;;;;;;;;;;;;;;;;;;;;

(defun string (string)
  "Returns a string described by the provided object."
  (declare (system::%java-class-name "jcl.strings.functions.String"))
  (ext:jinvoke-static
    (ext:jmethod "toLispString" (ext:jclass "jcl.lang.StringStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    string))

;;;;;;;;;;;;;;;;;;;;;;

(defun make-string (size &key (initial-element #\NULL) (element-type 'character))
  "Returns a simple string of length size whose elements have been initialized to initial-element."
  (declare (system::%java-class-name "jcl.strings.functions.String"))
  (ext:jinvoke-static
    (ext:jmethod "makeString" (ext:jclass "jcl.lang.StringStruct")
                 (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.SymbolStruct"))
    size initial-element element-type))

;;;;;;;;;;;;;;;;;;;;;;

(defun char (string index)
  "Char accesses the element of string specified by index."
  (declare (system::%java-class-name "jcl.strings.functions.Char"))
  ($char_ string index))

(defun schar (string index)
  "Schar accesses the element of string specified by index."
  (declare (system::%java-class-name "jcl.strings.functions.Schar"))
  ($schar string index))

;;;;;;;;;;;;;;;;;;;;;;

(defun string-trim (character-bag string)
  "Returns a substring of string, with all characters in character-bag stripped off the beginning and end."
  (declare (system::%java-class-name "jcl.strings.functions.StringTrim"))
  (let ((string (string string)))
    ($stringTrim string character-bag)))

(defun string-left-trim (character-bag string)
  "Returns a substring of string, with all characters in character-bag stripped off the beginning."
  (declare (system::%java-class-name "jcl.strings.functions.StringLeftTrim"))
  (let ((string (string string)))
    ($stringLeftTrim string character-bag)))

(defun string-right-trim (character-bag string)
  "Returns a substring of string, with all characters in character-bag stripped off the end."
  (declare (system::%java-class-name "jcl.strings.functions.StringRightTrim"))
  (let ((string (string string)))
    ($stringRightTrim string character-bag)))

;;;;;;;;;;;;;;;;;;;;;;

(defun string-upcase (string &key (start 0) end)
  "Returns a string just like string with all lowercase characters replaced by the corresponding uppercase characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringUpcase"))
  (let* ((string (string string))
         (end (or end (length string))))
    ($stringUpcase string start end)))

(defun string-downcase (string &key (start 0) end)
  "Returns a string just like string with all uppercase characters replaced by the corresponding lowercase characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringDowncase"))
  (let* ((string (string string))
         (end (or end (length string))))
    ($stringDowncase string start end)))

(defun string-capitalize (string &key (start 0) end)
  "Produces a copy of string such that, for every word in the copy, the first character of the ``word,'' if it has case,
   is uppercase and any other characters with case in the word are lowercase."
  (declare (system::%java-class-name "jcl.strings.functions.StringCapitalize"))
  (let* ((string (string string))
         (end (or end (length string))))
    ($stringCapitalize string start end)))

(defun nstring-upcase (string &key (start 0) end)
  "Returns the provided string modified with all lowercase characters replaced by the corresponding uppercase characters."
  (declare (system::%java-class-name "jcl.strings.functions.NStringUpcase"))
  (let* ((string (string string))
         (end (or end (length string))))
    ($nStringUpcase string start end)))

(defun nstring-downcase (string &key (start 0) end)
  "Returns the provided string modified with all uppercase characters replaced by the corresponding lowercase characters."
  (declare (system::%java-class-name "jcl.strings.functions.NStringDowncase"))
  (let* ((string (string string))
         (end (or end (length string))))
    ($nStringDowncase string start end)))

(defun nstring-capitalize (string &key (start 0) end)
  "Modifies the provided string such that, for every word in the copy, the first character of the ``word,'' if it has case,
  is uppercase and any other characters with case in the word are lowercase."
  (declare (system::%java-class-name "jcl.strings.functions.NStringCapitalize"))
  (let* ((string (string string))
         (end (or end (length string))))
    ($nStringCapitalize string start end)))

;;;;;;;;;;;;;;;;;;;;;;

(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String= is true if the supplied substrings are of the same length and contain the same characters in corresponding
  positions; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringEqualTo"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringEqual string1 string2 start1 end1 start2 end2)))

(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String/= is true if the supplied substrings are different; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringNotEqualTo"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringNotEqual string1 string2 start1 end1 start2 end2)))

(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String< is true if substring1 is less than substring2; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringLessThan"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringLessThan string1 string2 start1 end1 start2 end2)))

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String> is true if substring1 is greater than substring2; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringGreaterThan"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringGreaterThan string1 string2 start1 end1 start2 end2)))

(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String<= is true if substring1 is less than or equal to substring2; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringLessThanOrEqualTo"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringLessThanOrEqualTo string1 string2 start1 end1 start2 end2)))

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String>= is true if substring1 is greater than or equal to substring2; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringGreaterThanOrEqualTo"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringGreaterThanOrEqualTo string1 string2 start1 end1 start2 end2)))

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-equal is just like string= except that differences in case are ignored; two characters are considered to be
  the same if char-equal is true of them."
  (declare (system::%java-class-name "jcl.strings.functions.StringEqual"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringEqualIgnoreCase string1 string2 start1 end1 start2 end2)))

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-not-equal is just like string/= except that differences in case are ignored; two characters are considered to
  be the same if char-not-equal is true of them."
  (declare (system::%java-class-name "jcl.strings.functions.StringNotEqual"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringNotEqualIgnoreCase string1 string2 start1 end1 start2 end2)))

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-lessp is exactly like string<, except that distinctions between uppercase and lowercase letters are ignored.
  It is as if char-lessp were used instead of char< for comparing characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringLessp"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringLessThanIgnoreCase string1 string2 start1 end1 start2 end2)))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-greaterp are exactly like string>, except that distinctions between uppercase and lowercase letters are
  ignored. It is as if char-greaterp were used instead of char> for comparing characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringGreaterp"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringGreaterThanIgnoreCase string1 string2 start1 end1 start2 end2)))

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-not-greaterp is exactly like string<=, except that distinctions between uppercase and lowercase letters are
  ignored. It is as if char-lessp were used instead of char< for comparing characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringNotGreaterp"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringLessThanOrEqualToIgnoreCase string1 string2 start1 end1 start2 end2)))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-not-lessp is exactly like string>=, except that distinctions between uppercase and lowercase letters are
  ignored. It is as if char-greaterp were used instead of char> for comparing characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringNotLessp"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    ($stringGreaterThanOrEqualToIgnoreCase string1 string2 start1 end1 start2 end2)))

;;;;;;;;;;;;;;;;;;;;;;

(export '(stringp simple-string-p
          string
          make-string
          char schar
          string-trim string-left-trim string-right-trim
          string-upcase string-downcase string-capitalize
          nstring-upcase nstring-downcase nstring-capitalize
          string= string/= string< string> string<= string>=
          string-equal string-not-equal string-lessp string-greaterp string-not-greaterp string-not-lessp)
        "COMMON-LISP")
;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
  (require "macros")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: stringp, simple-string-p

;;;;;;;;;;;;;;;;;;;;;;

(defun string (string)
  "Returns a string described by the provided object."
  (declare (system::%java-class-name "jcl.strings.functions.String"))
  (ext:jinvoke-static
    (ext:jmethod "fromDesignator" (ext:jclass "jcl.lang.StringStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    string))

;;;;;;;;;;;;;;;;;;;;;;

(defun make-string (size &key (initial-element #\NULL) (element-type 'character))
  "Returns a simple string of length size whose elements have been initialized to initial-element."
  (declare (system::%java-class-name "jcl.strings.functions.MakeString"))
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
  (ext:jinvoke-interface
    (ext:jmethod "char_" (ext:jclass "jcl.lang.StringStruct")
                 (ext:jclass "jcl.lang.FixnumStruct"))
    string index))

(defun schar (string index)
  "Schar accesses the element of string specified by index."
  (declare (system::%java-class-name "jcl.strings.functions.Schar"))
  (ext:jinvoke-interface
    (ext:jmethod "schar" (ext:jclass "jcl.lang.StringStruct")
                 (ext:jclass "jcl.lang.FixnumStruct"))
    string index))

;;;;;;;;;;;;;;;;;;;;;;

(defun string-trim (character-bag string)
  "Returns a substring of string, with all characters in character-bag stripped off the beginning and end."
  (declare (system::%java-class-name "jcl.strings.functions.StringTrim"))
  (let ((string (string string)))
    (ext:jinvoke-interface
      (ext:jmethod "stringTrim" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.SequenceStruct"))
      string character-bag)))

(defun string-left-trim (character-bag string)
  "Returns a substring of string, with all characters in character-bag stripped off the beginning."
  (declare (system::%java-class-name "jcl.strings.functions.StringLeftTrim"))
  (let ((string (string string)))
    (ext:jinvoke-interface
      (ext:jmethod "stringLeftTrim" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.SequenceStruct"))
      string character-bag)))

(defun string-right-trim (character-bag string)
  "Returns a substring of string, with all characters in character-bag stripped off the end."
  (declare (system::%java-class-name "jcl.strings.functions.StringRightTrim"))
  (let ((string (string string)))
    (ext:jinvoke-interface
      (ext:jmethod "stringRightTrim" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.SequenceStruct"))
      string character-bag)))

;;;;;;;;;;;;;;;;;;;;;;

(defun string-upcase (string &key (start 0) end)
  "Returns a string just like string with all lowercase characters replaced by the corresponding uppercase characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringUpcase"))
  (let* ((string (string string))
         (end (or end (length string))))
    (ext:jinvoke-interface
      (ext:jmethod "stringUpcase" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string start end)))

(defun string-downcase (string &key (start 0) end)
  "Returns a string just like string with all uppercase characters replaced by the corresponding lowercase characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringDowncase"))
  (let* ((string (string string))
         (end (or end (length string))))
    (ext:jinvoke-interface
      (ext:jmethod "stringDowncase" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string start end)))

(defun string-capitalize (string &key (start 0) end)
  "Produces a copy of string such that, for every word in the copy, the first character of the ``word,'' if it has case,
   is uppercase and any other characters with case in the word are lowercase."
  (declare (system::%java-class-name "jcl.strings.functions.StringCapitalize"))
  (let* ((string (string string))
         (end (or end (length string))))
    (ext:jinvoke-interface
      (ext:jmethod "stringCapitalize" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string start end)))

(defun nstring-upcase (string &key (start 0) end)
  "Returns the provided string modified with all lowercase characters replaced by the corresponding uppercase characters."
  (declare (system::%java-class-name "jcl.strings.functions.NStringUpcase"))
  (let* ((string (string string))
         (end (or end (length string))))
    (ext:jinvoke-interface
      (ext:jmethod "nStringUpcase" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string start end)))

(defun nstring-downcase (string &key (start 0) end)
  "Returns the provided string modified with all uppercase characters replaced by the corresponding lowercase characters."
  (declare (system::%java-class-name "jcl.strings.functions.NStringDowncase"))
  (let* ((string (string string))
         (end (or end (length string))))
    (ext:jinvoke-interface
      (ext:jmethod "nStringDowncase" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string start end)))

(defun nstring-capitalize (string &key (start 0) end)
  "Modifies the provided string such that, for every word in the copy, the first character of the ``word,'' if it has case,
  is uppercase and any other characters with case in the word are lowercase."
  (declare (system::%java-class-name "jcl.strings.functions.NStringCapitalize"))
  (let* ((string (string string))
         (end (or end (length string))))
    (ext:jinvoke-interface
      (ext:jmethod "nStringCapitalize" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string start end)))

;;;;;;;;;;;;;;;;;;;;;;

(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String= is true if the supplied substrings are of the same length and contain the same characters in corresponding
  positions; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringEqualTo"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringEqual" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String/= is true if the supplied substrings are different; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringNotEqualTo"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringNotEqual" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String< is true if substring1 is less than substring2; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringLessThan"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringLessThan" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String> is true if substring1 is greater than substring2; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringGreaterThan"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringGreaterThan" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String<= is true if substring1 is less than or equal to substring2; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringLessThanOrEqualTo"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringLessThanOrEqualTo" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String>= is true if substring1 is greater than or equal to substring2; otherwise it is false."
  (declare (system::%java-class-name "jcl.strings.functions.StringGreaterThanOrEqualTo"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringGreaterThanOrEqualTo" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-equal is just like string= except that differences in case are ignored; two characters are considered to be
  the same if char-equal is true of them."
  (declare (system::%java-class-name "jcl.strings.functions.StringEqual"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringEqualIgnoreCase" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-not-equal is just like string/= except that differences in case are ignored; two characters are considered to
  be the same if char-not-equal is true of them."
  (declare (system::%java-class-name "jcl.strings.functions.StringNotEqual"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringNotEqualIgnoreCase" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-lessp is exactly like string<, except that distinctions between uppercase and lowercase letters are ignored.
  It is as if char-lessp were used instead of char< for comparing characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringLessp"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringLessThanIgnoreCase" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-greaterp are exactly like string>, except that distinctions between uppercase and lowercase letters are
  ignored. It is as if char-greaterp were used instead of char> for comparing characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringGreaterp"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringGreaterThanIgnoreCase" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-not-greaterp is exactly like string<=, except that distinctions between uppercase and lowercase letters are
  ignored. It is as if char-lessp were used instead of char< for comparing characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringNotGreaterp"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringLessThanOrEqualToIgnoreCase" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "String-not-lessp is exactly like string>=, except that distinctions between uppercase and lowercase letters are
  ignored. It is as if char-greaterp were used instead of char> for comparing characters."
  (declare (system::%java-class-name "jcl.strings.functions.StringNotLessp"))
  (let* ((string1 (string string1))
         (string2 (string string2))
         (end1 (or end1 (length string1)))
         (end2 (or end2 (length string2))))
    (ext:jinvoke-interface
      (ext:jmethod "stringGreaterThanOrEqualToIgnoreCase" (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct")
                   (ext:jclass "jcl.lang.IntegerStruct"))
      string1 string2 start1 end1 start2 end2)))

;;;;;;;;;;;;;;;;;;;;;;

(provide "strings")
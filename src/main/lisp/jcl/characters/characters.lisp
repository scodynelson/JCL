;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: defvar char-code-limit
#|
(defconstant char-code-limit
  #-unicode 256
  #+unicode 65536
  "The upper exclusive bound on values produced by CHAR-CODE.")
|#
;;;;;;;;;;;;;;;;;;;;;;

(defun character (character)
  "Returns the character denoted by the character designator."
  (declare (system::%java-class-name "jcl.characters.functions.Character"))
  (ext:jinvoke-static
    (ext:jmethod "fromDesignator" (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    character))

;;;;;;;;;;;;;;;;;;;;;;

(defun char-name (character)
  "Returns a string that is the name of the character, or nil if the character has no name."
  (declare (system::%java-class-name "jcl.characters.functions.CharName"))
  (ext:jinvoke-interface
    (ext:jmethod "charName" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

(defun name-char (name)
  "Returns the character object whose name is name. If such a character does not exist, nil is returned."
  (declare (system::%java-class-name "jcl.characters.functions.NameChar"))
  (ext:jinvoke-static
    (ext:jmethod "nameChar" (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    name))

(defun char-code (character)
  "Returns the code attribute of character."
  (declare (system::%java-class-name "jcl.characters.functions.CharCode"))
  (ext:jinvoke-interface
    (ext:jmethod "charCode" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

(defun code-char (code)
  "Returns a character with the code attribute given by code. If no such character exists and one cannot be created,
  nil is returned."
  (declare (system::%java-class-name "jcl.characters.functions.CodeChar"))
  (ext:jinvoke-static
    (ext:jmethod "codeChar" (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    code))

(defun char-int (character)
  "Returns a non-negative integer encoding the character object."
  (declare (system::%java-class-name "jcl.characters.functions.CharInt"))
  (ext:jinvoke-interface
    (ext:jmethod "charInt" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

(defun digit-char (weight &optional (radix 10))
  "If weight is less than radix, digit-char returns a character which has that weight when considered as a digit in the
  specified radix. If the resulting character is to be an alphabetic[1] character, it will be an uppercase character.
  If weight is greater than or equal to radix, digit-char returns false."
  (declare (system::%java-class-name "jcl.characters.functions.DigitChar"))
  (ext:jinvoke-static
    (ext:jmethod "digitChar" (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    weight radix))

(defun digit-char-p (character &optional (radix 10))
  "Tests whether character is a digit in the specified radix. If it is a digit in that radix, its weight is returned as
  an integer; otherwise nil is returned."
  (declare (system::%java-class-name "jcl.characters.functions.DigitCharP"))
  (ext:jinvoke-interface
    (ext:jmethod "charDigit" (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    character radix))

;;;;;;;;;;;;;;;;;;;;;;

(defun char-upcase (character)
  "Returns the corresponding uppercase character."
  (declare (system::%java-class-name "jcl.characters.functions.CharUpcase"))
  (ext:jinvoke-interface
    (ext:jmethod "charUpcase" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

(defun char-downcase (character)
  "Returns the corresponding lowercase character."
  (declare (system::%java-class-name "jcl.characters.functions.CharDowncase"))
  (ext:jinvoke-interface
    (ext:jmethod "charDowncase" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

;;;;;;;;;;;;;;;;;;;;;;

(defun alpha-char-p (character)
  "Returns true if character is an alphabetic character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.AlphaCharP"))
  (ext:jinvoke-interface
    (ext:jmethod "isAlphaChar" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

(defun alphanumericp (character)
  "Returns true if character is an alphabetic character or a numeric character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.AlphaNumericP"))
  (ext:jinvoke-interface
    (ext:jmethod "isAlphanumeric" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

(defun graphic-char-p (character)
  "Returns true if character is a graphic character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.GraphicCharP"))
  (ext:jinvoke-interface
    (ext:jmethod "isGraphicChar" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

(defun standard-char-p (character)
  "Returns true if character is a standard character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.StandardCharP"))
  (ext:jinvoke-interface
    (ext:jmethod "isStandardChar" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

(defun upper-case-p (character)
  "Returns true if character is an uppercase character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.UpperCaseP"))
  (ext:jinvoke-interface
    (ext:jmethod "isUpperCase" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

(defun lower-case-p (character)
  "Returns true if character is a lowercase character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.LowerCaseP"))
  (ext:jinvoke-interface
    (ext:jmethod "isLowerCase" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

(defun both-case-p (character)
  "Returns true if character is a character with case; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.BothCaseP"))
  (ext:jinvoke-interface
    (ext:jmethod "isBothCase" (ext:jclass "jcl.lang.CharacterStruct"))
    character))

;;;;;;;;;;;;;;;;;;;;;;

(defun char= (character &rest characters)
  "Returns true if all characters are the same; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharEq"))
  (char-comparator-util "isEqualTo" character characters))

(defun char/= (character &rest characters)
  "Returns true if all characters are different; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharNotEq"))
  (char-comparator-util "isNotEqualTo" character characters))

(defun char< (character &rest characters)
  "Returns true if the characters are monotonically increasing; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharLT"))
  (char-comparator-util "isLessThan" character characters))

(defun char> (character &rest characters)
  "Returns true if the characters are monotonically decreasing; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharGT"))
  (char-comparator-util "isGreaterThan" character characters))

(defun char<= (character &rest characters)
  "Returns true if the characters are monotonically non-decreasing; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharLTEq"))
  (char-comparator-util "isLessThanOrEqualTo" character characters))

(defun char>= (character &rest characters)
  "Returns true if the characters are monotonically non-increasing; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharGTEq"))
  (char-comparator-util "isGreaterThanOrEqualTo" character characters))

(defun char-equal (character &rest characters)
  "Returns true if all characters are the same, ignoring differences in case; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharEqual"))
  (char-comparator-util "isEqualToIgnoreCase" character characters))

(defun char-not-equal (character &rest characters)
  "Returns true if all characters are different, ignoring differences in case; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharNotEqual"))
  (char-comparator-util "isNotEqualToIgnoreCase" character characters))

(defun char-lessp (character &rest characters)
  "Returns true if the characters are monotonically increasing, ignoring differences in case; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharLessP"))
  (char-comparator-util "isLessThanIgnoreCase" character characters))

(defun char-greaterp (character &rest characters)
  "Returns true if the characters are monotonically decreasing, ignoring differences in case; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharGreaterP"))
  (char-comparator-util "isGreaterThanIgnoreCase" character characters))

(defun char-not-greaterp (character &rest characters)
  "Returns true if the characters are monotonically non-decreasing, ignoring differences in case; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharNotGreaterP"))
  (char-comparator-util "isLessThanOrEqualToIgnoreCase" character characters))

(defun char-not-lessp (character &rest characters)
  "Returns true if the characters are monotonically non-increasing, ignoring differences in case; otherwise, it returns false."
  (declare (system::%java-class-name "jcl.characters.functions.CharNotLessP"))
  (char-comparator-util "isGreaterThanOrEqualToIgnoreCase" character characters))

(defmacro char-comparator-util (comparison-method-name character characters)
  `(ext:jinvoke-static
    (ext:jmethod ,comparison-method-name (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           (cons ,character ,characters))))

;;;;;;;;;;;;;;;;;;;;;;

(provide "characters")
;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
#|
(eval-when (:load-toplevel :execute)
(export '()
        (find-package "COMMON-LISP"))
) ;eval-when
|#
(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: defvar char-code-limit

;;;;;;;;;;;;;;;;;;;;;;

(defun character (character)
  "Returns the character denoted by the character designator."
  (declare (system::%java-class-name "jcl.characters.functions.Character"))
  (ext:jinvoke-static
    (ext:jmethod "toLispCharacter" (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    character))

;;;;;;;;;;;;;;;;;;;;;;

(defun char-name (character)
  "Returns a string that is the name of the character, or nil if the character has no name."
  (declare (system::%java-class-name "jcl.characters.functions.CharName"))
  ($charName character))

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
  ($charCode character))

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
  ($charInt character))

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
  ($charDigit character radix))

;;;;;;;;;;;;;;;;;;;;;;

(defun char-upcase (character)
  "Returns the corresponding uppercase character."
  (declare (system::%java-class-name "jcl.characters.functions.CharUpcase"))
  ($charUpcase character))

(defun char-downcase (character)
  "Returns the corresponding lowercase character."
  (declare (system::%java-class-name "jcl.characters.functions.CharDowncase"))
  ($charDowncase character))

;;;;;;;;;;;;;;;;;;;;;;

(defun alpha-char-p (character)
  "Returns true if character is an alphabetic character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.AlphaCharP"))
  ($isAlphaChar character))

(defun alphanumericp (character)
  "Returns true if character is an alphabetic character or a numeric character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.AlphaNumericP"))
  ($isAlphanumeric character))

(defun graphic-char-p (character)
  "Returns true if character is a graphic character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.GraphicCharP"))
  ($isGraphicChar character))

(defun standard-char-p (character)
  "Returns true if character is a standard character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.StandardCharP"))
  ($isStandardChar character))

(defun upper-case-p (character)
  "Returns true if character is an uppercase character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.UpperCaseP"))
  ($isUpperCase character))

(defun lower-case-p (character)
  "Returns true if character is a lowercase character; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.LowerCaseP"))
  ($isLowerCase character))

(defun both-case-p (character)
  "Returns true if character is a character with case; otherwise, returns false."
  (declare (system::%java-class-name "jcl.characters.functions.BothCaseP"))
  ($isBothCase character))

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

;; TODO: Use Macro??
(defun char-comparator-util (comparison-method-name character characters)
  (ext:jinvoke-static
    (ext:jmethod comparison-method-name (ext:jclass "jcl.lang.CharacterStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                 (cons character characters))))

;;;;;;;;;;;;;;;;;;;;;;

(export '(char-code-limit
          character
          char-name name-char char-code code-char char-int digit-char digit-char-p
          char-upcase char-downcase
          alpha-char-p alphanumericp graphic-char-p standard-char-p upper-case-p lower-case-p both-case-p
          char= char/= char< char> char<= char>=
          char-equal char-not-equal char-lessp char-greaterp char-not-greaterp char-not-lessp)
        "COMMON-LISP")
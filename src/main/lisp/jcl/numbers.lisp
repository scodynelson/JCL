;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
  (require "macros")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: most-positive-fixnum
;; TODO: most-negative-fixnum

;; TODO: float most-positive/most-negative constants
;; TODO: float epsilon constants

;;;;;;;;;;;;;;;;;;;;;;

(defun abs (number)
  "Returns the absolute value of number."
  (declare (system::%java-class-name "jcl.numbers.functions.Abs"))
  (ext:jinvoke-interface
    (ext:jmethod "abs" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun conjugate (number)
  "Returns the complex conjugate of number. The conjugate of a real number is itself."
  (declare (system::%java-class-name "jcl.numbers.functions.Conjugate"))
  (ext:jinvoke-interface
    (ext:jmethod "conjugate" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun exp (power-number)
  "Returns e raised to the power power-number, where e is the base of the natural logarithms."
  (declare (system::%java-class-name "jcl.numbers.functions.Exp"))
  (ext:jinvoke-interface
    (ext:jmethod "exp" (ext:jclass "jcl.lang.NumberStruct"))
    power-number))

(defun expt (base-number power-number)
  "Returns base-number raised to the power power-number."
  (declare (system::%java-class-name "jcl.numbers.functions.Expt"))
  (ext:jinvoke-interface
    (ext:jmethod "expt" (ext:jclass "jcl.lang.NumberStruct")
                 (ext:jclass "jcl.lang.NumberStruct"))
    base-number power-number))

(defun log (number &optional (base nil basep))
  "Returns the logarithm of number in base base. If base is not supplied its value is e, the base of the natural logarithms."
  (declare (system::%java-class-name "jcl.numbers.functions.Log"))
  (if basep
      (ext:jinvoke-interface
        (ext:jmethod "log" (ext:jclass "jcl.lang.NumberStruct")
                     (ext:jclass "jcl.lang.NumberStruct"))
        number base)
    (ext:jinvoke-interface
      (ext:jmethod "log" (ext:jclass "jcl.lang.NumberStruct"))
      number)))

(defun signum (number)
  "Determines a numerical value that indicates whether number is negative, zero, or positive."
  (declare (system::%java-class-name "jcl.numbers.functions.Signum"))
  (ext:jinvoke-interface
    (ext:jmethod "signum" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun sqrt (number)
  "Returns the principal square root of number."
  (declare (system::%java-class-name "jcl.numbers.functions.Sqrt"))
  (ext:jinvoke-interface
    (ext:jmethod "sqrt" (ext:jclass "jcl.lang.NumberStruct"))
    number))

;;;;;;;;;;;;;;;;;;;;;;

(defun complex (realpart &optional (imagpart 1 imagpart-p))
  "Returns a number whose real part is realpart and whose imaginary part is imagpart."
  (declare (system::%java-class-name "jcl.numbers.functions.Complex"))
  (if imagpart-p
      (ext:jinvoke-static
          (ext:jmethod "toLispComplex" (ext:jclass "jcl.lang.ComplexStruct")
                       (ext:jclass "jcl.lang.RealStruct")
                       (ext:jclass "jcl.lang.RealStruct"))
          realpart
          imagpart)
    (if (rationalp realpart)
        realpart
      (ext:jinvoke-static
          (ext:jmethod "toLispComplex" (ext:jclass "jcl.lang.ComplexStruct")
                       (ext:jclass "jcl.lang.RealStruct")
                       (ext:jclass "jcl.lang.RealStruct"))
          realpart
          1.0))))

;;;;;;;;;;;;;;;;;;;;;;

(defun cis (real)
  "Returns the value of e^i* radians, which is a complex in which the real part is equal to the cosine of radians, and the imaginary part is equal to the sine of radians."
  (declare (system::%java-class-name "jcl.numbers.functions.Cis"))
  (ext:jinvoke-interface
    (ext:jmethod "cis" (ext:jclass "jcl.lang.RealStruct"))
    real))

(defun max (real &rest reals)
  "Returns the real that is greatest (closest to positive infinity)"
  (declare (system::%java-class-name "jcl.numbers.functions.Max"))
  (ext:jinvoke-static
    (ext:jmethod "max" (ext:jclass "jcl.lang.RealStruct")
                 (ext:jclass "jcl.lang.RealStruct")
                 (ext:jclass "java.util.List"))
    real
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           reals)))

(defun min (real)
  "Returns the real that is least (closest to negative infinity)."
  (declare (system::%java-class-name "jcl.numbers.functions.Min"))
  (ext:jinvoke-static
    (ext:jmethod "min" (ext:jclass "jcl.lang.RealStruct")
                 (ext:jclass "jcl.lang.RealStruct")
                 (ext:jclass "java.util.List"))
    real
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           reals)))

(defun mod (real divisor)
  "Performs the operation floor on number and divisor and returns the remainder of the floor operation."
  (declare (system::%java-class-name "jcl.numbers.functions.Mod"))
  (ext:jinvoke-interface
    (ext:jmethod "mod" (ext:jclass "jcl.lang.RealStruct")
                 (ext:jclass "jcl.lang.RealStruct"))
    real divisor))

(defun rem (real divisor)
  "Performs the operation truncate on number and divisor and returns the remainder of the truncate operation."
  (declare (system::%java-class-name "jcl.numbers.functions.Rem"))
  (ext:jinvoke-interface
    (ext:jmethod "rem" (ext:jclass "jcl.lang.RealStruct")
                 (ext:jclass "jcl.lang.RealStruct"))
    real divisor))

(defun rational (real)
  "Convert real to a rational."
  (declare (system::%java-class-name "jcl.numbers.functions.Rational"))
  (ext:jinvoke-interface
    (ext:jmethod "rational" (ext:jclass "jcl.lang.RealStruct"))
    real))

(defun rationalize (real)
  "Convert real to a rational."
  (declare (system::%java-class-name "jcl.numbers.functions.Rationalize"))
  ;; TODO: Do we do anything different here???
  (ext:jinvoke-interface
    (ext:jmethod "rational" (ext:jclass "jcl.lang.RealStruct"))
    real))

;;;;;;;;;;;;;;;;;;;;;;

(defun decode-float (float)
  "Computes three values that characterize float."
  (declare (system::%java-class-name "jcl.numbers.functions.DecodeFloat"))
  (ext:jinvoke-virtual
    (ext:jmethod "toValues" (ext:jclass "jcl.lang.DecodeFloatResult"))
    (ext:jinvoke-interface
      (ext:jmethod "decodeFloat" (ext:jclass "jcl.lang.FloatStruct"))
      float)))

(defun integer-decode-float (float)
  "Computes three values that characterize float."
  (declare (system::%java-class-name "jcl.numbers.functions.IntegerDecodeFloat"))
  (ext:jinvoke-virtual
    (ext:jmethod "toValues" (ext:jclass "jcl.lang.DecodeFloatResult"))
    (ext:jinvoke-interface
      (ext:jmethod "integerDecodeFloat" (ext:jclass "jcl.lang.FloatStruct"))
      float)))

(defun float (real &optional (prototype nil prototype-p))
  "Returns a number z such that z and float-1 have the same sign and also such that z and float-2 have the same absolute value."
  (declare (system::%java-class-name "jcl.numbers.functions.Float"))
  (if prototype-p
      (ext:jinvoke-interface
        (ext:jmethod "floatingPoint" (ext:jclass "jcl.lang.RealStruct")
                     (ext:jclass "jcl.lang.FloatStruct"))
        float prototype)
    (ext:jinvoke-interface
      (ext:jmethod "floatingPoint" (ext:jclass "jcl.lang.RealStruct"))
      prototype)))

(defun float-digits (float)
  "Returns the number of radix b digits used in the representation of float."
  (declare (system::%java-class-name "jcl.numbers.functions.FloatDigits"))
  (ext:jinvoke-interface
    (ext:jmethod "floatDigits" (ext:jclass "jcl.lang.FloatStruct"))
    float))

(defun float-precision (float)
  "Returns the number of significant radix b digits present in float; if float is a float zero, then the result is an integer zero."
  (declare (system::%java-class-name "jcl.numbers.functions.FloatPrecision"))
  (ext:jinvoke-interface
    (ext:jmethod "floatPrecision" (ext:jclass "jcl.lang.FloatStruct"))
    float))

(defun float-sign (float1 &optional (float2 nil float2-p))
  "Returns a number z such that z and float-1 have the same sign and also such that z and float-2 have the same absolute value."
  (declare (system::%java-class-name "jcl.numbers.functions.FloatSign"))
  (if float2-p
      (ext:jinvoke-interface
        (ext:jmethod "floatSign" (ext:jclass "jcl.lang.FloatStruct")
                     (ext:jclass "jcl.lang.FloatStruct"))
        float1 float2)
    (ext:jinvoke-interface
      (ext:jmethod "floatSign" (ext:jclass "jcl.lang.FloatStruct"))
      float1)))

(defun scale-float (float scale)
  "Returns (* float (expt (float b float) integer)), where b is the radix of the floating-point representation. float is not necessarily between 1/b and 1."
  (declare (system::%java-class-name "jcl.numbers.functions.ScaleFloat"))
  (ext:jinvoke-interface
    (ext:jmethod "scaleFloat" (ext:jclass "jcl.lang.FloatStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    float scale))

;;;;;;;;;;;;;;;;;;;;;;

(defun ash (integer count)
  "Shifts integer arithmetically left by count bit positions if count is positive, or right count bit positions if count is negative."
  (declare (system::%java-class-name "jcl.numbers.functions.Ash"))
  (ext:jinvoke-interface
    (ext:jmethod "ash" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    integer count))

(defun gcd (&rest integers)
  "Returns the greatest common divisor of integers. If only one integer is supplied, its absolute value is returned. If no integers are given, gcd returns 0."
  (declare (system::%java-class-name "jcl.numbers.functions.Gcd"))
  (ext:jinvoke-static
    (ext:jmethod "gcd" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           integers)))

(defun lcm (&rest integers)
  "Returns the least common multiple of integers. If no integers are given, lcm returns 1"
  (declare (system::%java-class-name "jcl.numbers.functions.Lcm"))
  (ext:jinvoke-static
    (ext:jmethod "lcm" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           integers)))

(defun integer-length (integer)
  "Returns the number of bits needed to represent integer in binary two's-complement format."
  (declare (system::%java-class-name "jcl.numbers.functions.IntegerLength"))
  (ext:jinvoke-interface
    (ext:jmethod "integerLength" (ext:jclass "jcl.lang.IntegerStruct"))
    integer))

(defun isqrt (integer)
  "Returns the greatest integer less than or equal to the exact positive square root of natural."
  (declare (system::%java-class-name "jcl.numbers.functions.ISqrt"))
  (ext:jinvoke-interface
    (ext:jmethod "isqrt" (ext:jclass "jcl.lang.IntegerStruct"))
    integer))

;;;;;;;;;;;;;;;;;;;;;;

(defun + (number &rest numbers)
  "Returns the sum of numbers, performing any necessary type conversions in the process. If no numbers are supplied, 0 is returned."
  (declare (system::%java-class-name "jcl.numbers.functions.Add"))
  (ext:jinvoke-static
    (ext:jmethod "add" (ext:jclass "jcl.lang.NumberStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           (cons number numbers))))

(defun - (number &rest numbers)
  "If only one number is supplied, the negation of that number is returned. If more than one argument is given, it subtracts all of the subtrahends from the minuend and returns the result."
  (declare (system::%java-class-name "jcl.numbers.functions.Subtract"))
  (ext:jinvoke-static
    (ext:jmethod "subtract" (ext:jclass "jcl.lang.NumberStruct")
                 (ext:jclass "jcl.lang.NumberStruct")
                 (ext:jclass "java.util.List"))
    number
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           numbers)))

(defun * (number &rest numbers)
  "Returns the product of numbers, performing any necessary type conversions in the process. If no numbers are supplied, 1 is returned."
  (declare (system::%java-class-name "jcl.numbers.functions.Multiply"))
  (ext:jinvoke-static
    (ext:jmethod "multiply" (ext:jclass "jcl.lang.NumberStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           (cons number numbers))))

(defun / (number &rest numbers)
  "If no denominators are supplied, the function / returns the reciprocal of number. If more than one argument is given, the function / divides the numerator by all of the denominators and returns the resulting quotient."
  (declare (system::%java-class-name "jcl.numbers.functions.Divide"))
  (ext:jinvoke-static
    (ext:jmethod "divide" (ext:jclass "jcl.lang.NumberStruct")
                 (ext:jclass "jcl.lang.NumberStruct")
                 (ext:jclass "java.util.List"))
    number
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           numbers)))

;;;;;;;;;;;;;;;;;;;;;;

(defun = (number &rest numbers)
  "Returns true if all numbers are the same in value; otherwise returns false."
  (declare (system::%java-class-name "jcl.numbers.functions.EqualTo"))
  (ext:jinvoke-static
    (ext:jmethod "isEqualTo" (ext:jclass "jcl.lang.NumberStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           (cons number numbers))))

(defun /= (number &rest numbers)
  "Returns true if no two numbers are the same in value; otherwise returns false."
  (declare (system::%java-class-name "jcl.numbers.functions.NotEqualTo"))
  (ext:jinvoke-static
    (ext:jmethod "isNotEqualTo" (ext:jclass "jcl.lang.NumberStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           (cons number numbers))))

(defun < (number &rest numbers)
  "Returns true if the numbers are in monotonically increasing order; otherwise returns false."
  (declare (system::%java-class-name "jcl.numbers.functions.LessThan"))
  (ext:jinvoke-static
    (ext:jmethod "isLessThan" (ext:jclass "jcl.lang.RealStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           (cons number numbers))))

(defun > (number &rest numbers)
  "Returns true if the numbers are in monotonically decreasing order; otherwise returns false."
  (declare (system::%java-class-name "jcl.numbers.functions.GreaterThan"))
  (ext:jinvoke-static
    (ext:jmethod "isGreaterThan" (ext:jclass "jcl.lang.RealStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           (cons number numbers))))

(defun <= (number &rest numbers)
  "Returns true if the numbers are in monotonically nondecreasing order; otherwise returns false."
  (declare (system::%java-class-name "jcl.numbers.functions.LessThanOrEqualTo"))
  (ext:jinvoke-static
    (ext:jmethod "isLessThanOrEqualTo" (ext:jclass "jcl.lang.RealStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           (cons number numbers))))

(defun >= (number &rest numbers)
  "Returns true if the numbers are in monotonically nonincreasing order; otherwise returns false."
  (declare (system::%java-class-name "jcl.numbers.functions.GreaterThanOrEqualTo"))
  (ext:jinvoke-static
    (ext:jmethod "isGreaterThanOrEqualTo" (ext:jclass "jcl.lang.RealStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           (cons number numbers))))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: def-constant pi

;;;;;;;;;;;;;;;;;;;;;;

(defun sin (radians)
  "Returns the sine of radians."
  (declare (system::%java-class-name "jcl.numbers.functions.Sin"))
  (ext:jinvoke-interface
    (ext:jmethod "sin" (ext:jclass "jcl.lang.NumberStruct"))
    radians))

(defun cos (radians)
  "Returns the cosine of radians."
  (declare (system::%java-class-name "jcl.numbers.functions.Cos"))
  (ext:jinvoke-interface
    (ext:jmethod "cos" (ext:jclass "jcl.lang.NumberStruct"))
    radians))

(defun tan (radians)
  "Returns the tangent of radians."
  (declare (system::%java-class-name "jcl.numbers.functions.Tan"))
  (ext:jinvoke-interface
    (ext:jmethod "tan" (ext:jclass "jcl.lang.NumberStruct"))
    radians))

(defun asin (number)
  "Returns the arc-sine of the number."
  (declare (system::%java-class-name "jcl.numbers.functions.ASin"))
  (ext:jinvoke-interface
    (ext:jmethod "asin" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun acos (number)
  "Returns the arc-cosine of the number."
  (declare (system::%java-class-name "jcl.numbers.functions.ACos"))
  (ext:jinvoke-interface
    (ext:jmethod "acos" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun atan (number1 &optional (number2 nil number2p))
  "Returns the arc-tangent of number. If both number1 and number2 are supplied, the result is the arc-tangent of number1/number2."
  (declare (system::%java-class-name "jcl.numbers.functions.ATan"))
  (if number2p
      (ext:jinvoke-interface
        (ext:jmethod "atan" (ext:jclass "jcl.lang.RealStruct")
                     (ext:jclass "jcl.lang.RealStruct"))
        number1 number2))
    (ext:jinvoke-interface
      (ext:jmethod "atan" (ext:jclass "jcl.lang.NumberStruct"))
      number1))

(defun sinh (number)
  "Returns the hyperbolic-sine of the number."
  (declare (system::%java-class-name "jcl.numbers.functions.SinH"))
  (ext:jinvoke-interface
    (ext:jmethod "sinh" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun cosh (number)
  "Returns the hyperbolic-cosine of the number."
  (declare (system::%java-class-name "jcl.numbers.functions.CosH"))
  (ext:jinvoke-interface
    (ext:jmethod "cosh" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun tanh (number)
  "Returns the hyperbolic-tangent of the number."
  (declare (system::%java-class-name "jcl.numbers.functions.TanH"))
  (ext:jinvoke-interface
    (ext:jmethod "tanh" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun asinh (number)
  "Returns the hyperbolic-arc-sine of the number."
  (declare (system::%java-class-name "jcl.numbers.functions.ASinH"))
  (ext:jinvoke-interface
    (ext:jmethod "asinh" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun acosh (number)
  "Returns the hyperbolic-arc-cosine of the number."
  (declare (system::%java-class-name "jcl.numbers.functions.ACosH"))
  (ext:jinvoke-interface
    (ext:jmethod "acosh" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun atanh (number)
  "Returns the hyperbolic-arc-tangent of the number."
  (declare (system::%java-class-name "jcl.numbers.functions.ATanH"))
  (ext:jinvoke-interface
    (ext:jmethod "atanh" (ext:jclass "jcl.lang.NumberStruct"))
    number))

;;;;;;;;;;;;;;;;;;;;;;

(defun evenp (integer)
  "Returns true if integer is even (divisible by two); otherwise, returns false."
  (declare (system::%java-class-name "jcl.numbers.functions.EvenP"))
  (ext:jinvoke-interface
    (ext:jmethod "evenp" (ext:jclass "jcl.lang.IntegerStruct"))
    integer))

(defun oddp (integer)
  "Returns true if integer is odd (not divisible by two); otherwise, returns false."
  (declare (system::%java-class-name "jcl.numbers.functions.OddP"))
  (ext:jinvoke-interface
    (ext:jmethod "oddp" (ext:jclass "jcl.lang.IntegerStruct"))
    integer))

;;;;;;;;;;;;;;;;;;;;;;

(defun realpart (number)
  "Returns the real part of number."
  (declare (system::%java-class-name "jcl.numbers.functions.RealPart"))
  (ext:jinvoke-interface
    (ext:jmethod "realPart" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun imagpart (number)
  "Returns the imaginary part of number."
  (declare (system::%java-class-name "jcl.numbers.functions.ImagPart"))
  (ext:jinvoke-interface
    (ext:jmethod "imagPart" (ext:jclass "jcl.lang.NumberStruct"))
    number))

;;;;;;;;;;;;;;;;;;;;;;

(defun numerator (rational)
  "Returns the numerator of the canonical form of rational."
  (declare (system::%java-class-name "jcl.numbers.functions.Numerator"))
  (ext:jinvoke-interface
    (ext:jmethod "numerator" (ext:jclass "jcl.lang.RationalStruct"))
    rational))

(defun denominator (rational)
  "Returns the denominator of the canonical form of rational."
  (declare (system::%java-class-name "jcl.numbers.functions.Denominator"))
  (ext:jinvoke-interface
    (ext:jmethod "denominator" (ext:jclass "jcl.lang.RationalStruct"))
    rational))

;;;;;;;;;;;;;;;;;;;;;;

(defun logand (&rest integers)
  "Returns the AND of the integers."
  (declare (system::%java-class-name "jcl.numbers.functions.LogAnd"))
  (ext:jinvoke-static
    (ext:jmethod "logAnd" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           integers)))

(defun logandc1 (integer-1 integer-2)
  "Returns the AND COMPLEMENT of integer-1 with integer-2."
  (declare (system::%java-class-name "jcl.numbers.functions.LogAndC1"))
  (ext:jinvoke-interface
    (ext:jmethod "logAndC1" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    integer-1 integer-2))

(defun logandc2 (integer-1 integer-2)
  "Returns the AND of integer-1 with COMPLEMENT of integer-2."
  (declare (system::%java-class-name "jcl.numbers.functions.LogAndC2"))
  (ext:jinvoke-interface
    (ext:jmethod "logAndC2" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    integer-1 integer-2))

(defun logeqv (&rest integers)
  "Returns the EQUIVALENCE (EXCLUSIVE-NOR) of the integers."
  (declare (system::%java-class-name "jcl.numbers.functions.LogEqv"))
  (ext:jinvoke-static
    (ext:jmethod "logEqv" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           integers)))

(defun logior (&rest integers)
  "Returns the INCLUSIVE-OR of the integers."
  (declare (system::%java-class-name "jcl.numbers.functions.LogIor"))
  (ext:jinvoke-static
    (ext:jmethod "logIor" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           integers)))

(defun lognand (integer-1 integer-2)
  "Returns the COMPLEMENT of integer-1 AND integer-2."
  (declare (system::%java-class-name "jcl.numbers.functions.LogNand"))
  (ext:jinvoke-interface
    (ext:jmethod "logNand" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    integer-1 integer-2))

(defun lognor (integer-1 integer-2)
  "Returns the COMPLEMENT of integer-1 OR integer-2."
  (declare (system::%java-class-name "jcl.numbers.functions.LogNor"))
  (ext:jinvoke-interface
    (ext:jmethod "logNor" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    integer-1 integer-2))

(defun lognot (integer)
  "Returns the COMPLEMENT of integer."
  (declare (system::%java-class-name "jcl.numbers.functions.LogNot"))
  (ext:jinvoke-interface
    (ext:jmethod "logNot" (ext:jclass "jcl.lang.IntegerStruct"))
    integer))

(defun logorc1 (integer-1 integer-2)
  "Returns the OR COMPLEMENT of integer-1 with integer-2"
  (declare (system::%java-class-name "jcl.numbers.functions.LogOrC1"))
  (ext:jinvoke-interface
    (ext:jmethod "logOrC1" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    integer-1 integer-2))

(defun logorc2 (integer-1 integer-2)
  "Returns the OR of integer-1 with COMPLEMENT of integer-2."
  (declare (system::%java-class-name "jcl.numbers.functions.LogOrC2"))
  (ext:jinvoke-interface
    (ext:jmethod "logOrC2" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    integer-1 integer-2))

(defun logxor (&rest integers)
  "Returns the EXCLUSIVE-OR of the integers."
  (declare (system::%java-class-name "jcl.numbers.functions.LogXor"))
  (ext:jinvoke-static
    (ext:jmethod "logXor" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           integers)))

(defun logbitp (index integer)
  "Returns true if the bit in integer whose index is index (that is, its weight is 2^index) is a one-bit; otherwise it is false."
  (declare (system::%java-class-name "jcl.numbers.functions.LogBitP"))
  (ext:jinvoke-interface
    (ext:jmethod "logBitP" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    integer index)) ;; Note: yes, these are switched here on purpose

(defun logcount (integer)
  "Computes and returns the number of bits in the two's-complement binary representation of integer that are `on' or `set'. If integer is negative, the 0 bits are counted; otherwise, the 1 bits are counted."
  (declare (system::%java-class-name "jcl.numbers.functions.LogCount"))
  (ext:jinvoke-interface
    (ext:jmethod "logCount" (ext:jclass "jcl.lang.IntegerStruct"))
    integer))

(defun logtest (integer-1 integer-2)
  "Returns true if any of the bits designated by the 1's in integer-1 is 1 in integer-2; otherwise it is false."
  (declare (system::%java-class-name "jcl.numbers.functions.LogTest"))
  (ext:jinvoke-interface
    (ext:jmethod "logTest" (ext:jclass "jcl.lang.IntegerStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    integer-1 integer-2))

;;;;;;;;;;;;;;;;;;;;;;

(defun floor (number &optional (divisor 1))
  "Produces a quotient that has been truncated toward negative infinity; that is, the quotient represents the largest mathematical integer that is not larger than the mathematical quotient."
  (declare (system::%java-class-name "jcl.numbers.functions.Floor"))
  (ext:jinvoke-virtual
    (ext:jmethod "toValues" (ext:jclass "jcl.lang.QuotientRemainder"))
    (ext:jinvoke-interface
      (ext:jmethod "floor" (ext:jclass "jcl.lang.RealStruct")
                   (ext:jclass "jcl.lang.RealStruct"))
      number divisor)))

(defun ffloor (number &optional (divisor 1))
  "Produces a quotient that has been truncated toward negative infinity; that is, the quotient represents the largest mathematical integer that is not larger than the mathematical quotient."
  (declare (system::%java-class-name "jcl.numbers.functions.FFloor"))
  (ext:jinvoke-virtual
    (ext:jmethod "toValues" (ext:jclass "jcl.lang.QuotientRemainder"))
    (ext:jinvoke-interface
      (ext:jmethod "ffloor" (ext:jclass "jcl.lang.RealStruct")
                   (ext:jclass "jcl.lang.RealStruct"))
      number divisor)))

(defun ceiling (number &optional (divisor 1))
  "Produce a quotient that has been truncated toward positive infinity; that is, the quotient represents the smallest mathematical integer that is not smaller than the mathematical result."
  (declare (system::%java-class-name "jcl.numbers.functions.Ceiling"))
  (ext:jinvoke-virtual
    (ext:jmethod "toValues" (ext:jclass "jcl.lang.QuotientRemainder"))
    (ext:jinvoke-interface
      (ext:jmethod "ceiling" (ext:jclass "jcl.lang.RealStruct")
                   (ext:jclass "jcl.lang.RealStruct"))
      number divisor)))

(defun fceiling (number &optional (divisor 1))
  "Produce a quotient that has been truncated toward positive infinity; that is, the quotient represents the smallest mathematical integer that is not smaller than the mathematical result."
  (declare (system::%java-class-name "jcl.numbers.functions.FCeiling"))
  (ext:jinvoke-virtual
    (ext:jmethod "toValues" (ext:jclass "jcl.lang.QuotientRemainder"))
    (ext:jinvoke-interface
      (ext:jmethod "fceiling" (ext:jclass "jcl.lang.RealStruct")
                   (ext:jclass "jcl.lang.RealStruct"))
      number divisor)))

(defun truncate (number &optional (divisor 1))
  "Produces a quotient that has been truncated towards zero; that is, the quotient represents the mathematical integer of the same sign as the mathematical quotient, and that has the greatest integral magnitude not greater than that of the mathematical quotient."
  (declare (system::%java-class-name "jcl.numbers.functions.Truncate"))
  (ext:jinvoke-virtual
    (ext:jmethod "toValues" (ext:jclass "jcl.lang.QuotientRemainder"))
    (ext:jinvoke-interface
      (ext:jmethod "truncate" (ext:jclass "jcl.lang.RealStruct")
                   (ext:jclass "jcl.lang.RealStruct"))
      number divisor)))

(defun ftruncate (number &optional (divisor 1))
  "Produces a quotient that has been truncated towards zero; that is, the quotient represents the mathematical integer of the same sign as the mathematical quotient, and that has the greatest integral magnitude not greater than that of the mathematical quotient."
  (declare (system::%java-class-name "jcl.numbers.functions.FTruncate"))
  (ext:jinvoke-virtual
    (ext:jmethod "toValues" (ext:jclass "jcl.lang.QuotientRemainder"))
    (ext:jinvoke-interface
      (ext:jmethod "ftruncate" (ext:jclass "jcl.lang.RealStruct")
                   (ext:jclass "jcl.lang.RealStruct"))
      number divisor)))

(defun round (number &optional (divisor 1))
  "Produces a quotient that has been rounded to the nearest mathematical integer; if the mathematical quotient is exactly halfway between two integers, (that is, it has the form integer+1/2), then the quotient has been rounded to the even (divisible by two) integer."
  (declare (system::%java-class-name "jcl.numbers.functions.Round"))
  (ext:jinvoke-virtual
    (ext:jmethod "toValues" (ext:jclass "jcl.lang.QuotientRemainder"))
    (ext:jinvoke-interface
      (ext:jmethod "round" (ext:jclass "jcl.lang.RealStruct")
                   (ext:jclass "jcl.lang.RealStruct"))
      number divisor)))

(defun fround (number &optional (divisor 1))
  "Produces a quotient that has been rounded to the nearest mathematical integer; if the mathematical quotient is exactly halfway between two integers, (that is, it has the form integer+1/2), then the quotient has been rounded to the even (divisible by two) integer."
  (declare (system::%java-class-name "jcl.numbers.functions.FRound"))
  (ext:jinvoke-virtual
    (ext:jmethod "toValues" (ext:jclass "jcl.lang.QuotientRemainder"))
    (ext:jinvoke-interface
      (ext:jmethod "fround" (ext:jclass "jcl.lang.RealStruct")
                   (ext:jclass "jcl.lang.RealStruct"))
      number divisor)))

;;;;;;;;;;;;;;;;;;;;;;

(defun zerop (number)
  "Returns true if number is zero; otherwise, returns false."
  (declare (system::%java-class-name "jcl.numbers.functions.ZeroP"))
  (ext:jinvoke-interface
    (ext:jmethod "zerop" (ext:jclass "jcl.lang.NumberStruct"))
    number))

(defun plusp (real)
  "Returns true if real is greater than zero; otherwise, returns false."
  (declare (system::%java-class-name "jcl.numbers.functions.PlusP"))
  (ext:jinvoke-interface
    (ext:jmethod "plusp" (ext:jclass "jcl.lang.RealStruct"))
    real))

(defun minusp (real)
  "Returns true if real is less than zero; otherwise, returns false."
  (declare (system::%java-class-name "jcl.numbers.functions.MinusP"))
  (ext:jinvoke-interface
    (ext:jmethod "minusp" (ext:jclass "jcl.lang.RealStruct"))
    real))

;;;;;;;;;;;;;;;;;;;;;;

(defun 1+ (number)
  "Returns a number that is one more than its argument number."
  (declare (system::%java-class-name "jcl.numbers.functions.OnePlus"))
  (+ number 1))

(defun 1- (number)
  "Returns a number that is one less than its argument number."
  (declare (system::%java-class-name "jcl.numbers.functions.OneMinus"))
  (- number 1))

;;;;;;;;;;;;;;;;;;;;;;
#|
;; TODO
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

;; TODO: deposit-field

(defun mask-field (bytespec integer)
  (let ((size (byte-size bytespec))
        (pos (byte-position bytespec)))
    (logand integer (ash (1- (ash 1 size)) pos))))
|#
;;;;;;;;;;;;;;;;;;;;;;
#|
;; TODO
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
|#
;;;;;;;;;;;;;;;;;;;;;;
#|
;; TODO: boole constants
;; TODO: boole function
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
|#
;;;;;;;;;;;;;;;;;;;;;;
#|
;; TODO
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
|#
;;;;;;;;;;;;;;;;;;;;;;
#|
;; TODO
(defun upgraded-complex-part-type (typespec &optional environment)
  (declare (ignore environment))
  (if (subtypep typespec 'REAL)
      typespec
    (error 'simple-error
           :format-control "The type ~S is not a subtype of ~S."
           :format-arguments (list typespec 'REAL))))
|#
;;;;;;;;;;;;;;;;;;;;;;

;; TODO: def-var *random-state*

(defun make-random-state (&optional state)
  "Creates a fresh object of type random-state suitable for use as the value of *random-state*."
  (declare (system::%java-class-name "jcl.numbers.functions.MakeRandomState"))
  (ext:jinvoke-static
    (ext:jmethod "makeRandomState" (ext:jclass "jcl.lang.RandomStateStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    state))

(defun random (limit &optional (random-state *random-state*))
  "Returns a pseudo-random number that is a non-negative number less than limit and of the same type as limit."
  (declare (system::%java-class-name "jcl.numbers.functions.Random"))
  (ext:jinvoke-interface
    (ext:jmethod "random" (ext:jclass "jcl.lang.RandomStateStruct")
                 (ext:jclass "jcl.lang.RealStruct"))
    random-state limit))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: arithmetic-error-operands
;; TODO: arithmetic-error-operation

;;;;;;;;;;;;;;;;;;;;;;

(provide "numbers")
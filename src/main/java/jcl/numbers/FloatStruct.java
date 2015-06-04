/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

import jcl.LispStruct;
import jcl.types.FloatType;
import jcl.types.SingleFloatType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct extends RealStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 4803312076840516559L;

	public static final FloatStruct ZERO = new FloatStruct(BigDecimal.valueOf(0.0));

	public static final FloatStruct MINUS_ZERO = new FloatStruct(BigDecimal.valueOf(-0.0));

	public static final FloatStruct ONE = new FloatStruct(BigDecimal.valueOf(1.0));

	public static final FloatStruct MINUS_ONE = new FloatStruct(BigDecimal.valueOf(-1.0));

	/**
	 * The internal {@link BigDecimal} containing the float contents.
	 */
	private final BigDecimal bigDecimal;

	/**
	 * Public constructor.
	 *
	 * @param doubleValue
	 * 		the value of the FloatStruct
	 */
	FloatStruct(final double doubleValue) {
		this(SingleFloatType.INSTANCE, new BigDecimal(doubleValue));
	}

	/**
	 * Public constructor.
	 *
	 * @param bigDecimal
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final BigDecimal bigDecimal) {
		this(SingleFloatType.INSTANCE, bigDecimal);
	}

	/**
	 * Public constructor.
	 *
	 * @param floatType
	 * 		a {@link FloatType} that represents the type of {@link FloatType}
	 * @param bigDecimal
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final FloatType floatType, final BigDecimal bigDecimal) {
		super(floatType, null, null);
		this.bigDecimal = bigDecimal;
	}

	/**
	 * Getter for float {@link #bigDecimal} property.
	 *
	 * @return float {@link #bigDecimal} property
	 */
	public BigDecimal getBigDecimal() {
		return bigDecimal;
	}

	@Override
	public boolean eql(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean equal(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean equalp(final LispStruct lispStruct) {
		return (lispStruct instanceof NumberStruct) && isEqualTo((NumberStruct) lispStruct);
	}

	@Override
	public RealStruct abs() {
		if (bigDecimal.signum() >= 0) {
			return this;
		}
		final BigDecimal negate = bigDecimal.negate();
		return new FloatStruct(negate);
	}

	@Override
	public boolean zerop() {
		return bigDecimal.signum() == 0;
	}

	@Override
	public boolean plusp() {
		return bigDecimal.signum() > 0;
	}

	@Override
	public boolean minusp() {
		return bigDecimal.signum() < 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		return FloatAddStrategy.INSTANCE.add(this, number);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		return FloatSubtractStrategy.INSTANCE.subtract(this, number);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		return FloatMultiplyStrategy.INSTANCE.multiply(this, number);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		return FloatDivideStrategy.INSTANCE.divide(this, number);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		return FloatEqualToStrategy.INSTANCE.equalTo(this, number);
	}

	@Override
	public boolean isLessThan(final RealStruct real) {
		return FloatLessThanStrategy.INSTANCE.lessThan(this, real);
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		return FloatGreaterThanStrategy.INSTANCE.greaterThan(this, real);
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		return FloatLessThanOrEqualToStrategy.INSTANCE.lessThanOrEqualTo(this, real);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		return FloatGreaterThanOrEqualToStrategy.INSTANCE.greaterThanOrEqualTo(this, real);
	}

	@Override
	public NumberStruct signum() {
		if (zerop()) {
			return this;
		} else if (plusp()) {
			return ONE;
		} else {
			return MINUS_ONE;
		}
	}

	@Override
	public RealStruct imagPart() {
		return ZERO;
	}

	@Override
	public NumberStruct negation() {
		if (equals(ZERO)) {
			return MINUS_ZERO;
		}
		if (equals(MINUS_ZERO)) {
			return ZERO;
		}
		return new FloatStruct(bigDecimal.negate());
	}

	@Override
	public NumberStruct reciprocal() {
		return ONE.divide(this);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			return ONE;
		}

		if (zerop() || isEqualTo(ONE)) {
			return this;
		}

		return RealExptStrategy.INSTANCE.expt(this, power);
	}

	@Override
	public double doubleValue() {
		return bigDecimal.doubleValue();
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return bigDecimal;
	}

	@Override
	public RealStruct zeroValue() {
		return ZERO;
	}

	@Override
	public RationalStruct rational() {
		// TODO: can we get rid of using the BigFractionUtil???
		final BigFraction bigFraction = BigFractionUtil.getBigFraction(bigDecimal);
		if (bigFraction.getDenominator().equals(BigInteger.ONE)) {
			return new IntegerStruct(bigFraction.getNumerator());
		}
		return new RatioStruct(bigFraction);
	}

	@Override
	public QuotientRemainderResult floor(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.floor(this, divisor);
	}

	@Override
	public QuotientRemainderResult ffloor(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.ffloor(this, divisor);
	}

	@Override
	public QuotientRemainderResult ceiling(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.ceiling(this, divisor);
	}

	@Override
	public QuotientRemainderResult fceiling(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.fceiling(this, divisor);
	}

	@Override
	public QuotientRemainderResult round(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.round(this, divisor);
	}

	@Override
	public QuotientRemainderResult fround(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.fround(this, divisor);
	}

/*
// NOTE: custom functions: 'single-float-bits'

(defconstant single-float-bias 126)	; Intel says 127
(defconstant single-float-exponent-byte (byte 8 23))
(defconstant single-float-significand-byte (byte 23 0))
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

;;; DECODE-SINGLE-DENORM  --  Internal
;;;
;;;    Handle the denormalized case of DECODE-SINGLE-FLOAT.  We call
;;; INTEGER-DECODE-SINGLE-DENORM and then make the result into a float.
;;;
(defun decode-single-denorm (x)
  (multiple-value-bind (sig exp sign)
		               (integer-decode-single-denorm x)
    (values (make-single-float
	          (dpb sig
	               vm:single-float-significand-byte
		           (dpb vm:single-float-bias
		                vm:single-float-exponent-byte
		                0)))
	        (truly-the fixnum (+ exp vm:single-float-digits))
	        (float sign x))))

;;; DECODE-SINGLE-FLOAT  --  Internal
;;;
;;;    Handle the single-float case of DECODE-FLOAT.  If an infinity or NAN,
;;; error.  If a denorm, call d-s-DENORM to handle it.
;;;
(defun decode-single-float (x)
  (let* ((bits (single-float-bits (abs x)))
		 (exp (ldb vm:single-float-exponent-byte bits))
	     (sign (float-sign x))
	     (biased (truly-the single-float-exponent
			                (- exp vm:single-float-bias))))
    (unless (<= exp vm:single-float-normal-exponent-max)
      (error (intl:gettext "Can't decode NAN or infinity: ~S.") x))
    (cond ((zerop x)
	       (values 0.0f0 biased sign))
		  ((< exp vm:single-float-normal-exponent-min)
		   (decode-single-denorm x))
		  (t
		   (values (make-single-float
			         (dpb vm:single-float-bias
				          vm:single-float-exponent-byte
				          bits))
			       biased
			       sign)))))
 */

	public DecodeFloatResult decodeFloat() {
		final DecodeFloatResult decodeFloatResult = integerDecodeFloat();
		final RealStruct significand = decodeFloatResult.getSignificand();
		final RealStruct exponent = decodeFloatResult.getExponent();
		final RealStruct sign = decodeFloatResult.getSign();
		return new DecodeFloatResult(
				new FloatStruct(significand.bigDecimalValue()),
				(RealStruct) exponent.add(new IntegerStruct(BigInteger.valueOf(53L))),
				sign.minusp() ? MINUS_ONE : ONE
				);
	}

	public FloatStruct scaleFloat(final IntegerStruct scale) {
		final double twoScaleBase = 2.0D;
		final double scaleDouble = scale.doubleValue();
		final double pow = FastMath.pow(twoScaleBase, scaleDouble);

		// NOTE: don't use 'valueOf' here. It will add extra leading zeros.
		final BigDecimal multiplicand = new BigDecimal(pow);
		final BigDecimal multiply = bigDecimal.multiply(multiplicand);
		return new FloatStruct(multiply);
	}

	public IntegerStruct floatRadix() {
		return IntegerStruct.TWO;
	}

	public FloatStruct floatSign() {
		return floatSign(ONE);
	}

	public FloatStruct floatSign(final FloatStruct float2) {
		if (minusp()) {
			if (float2.minusp()) {
				return float2;
			} else {
				final BigDecimal subtract = BigDecimal.ZERO.subtract(float2.bigDecimal);
				return new FloatStruct(subtract);
			}
		} else {
			return (FloatStruct) float2.abs();
		}
	}

	public IntegerStruct floatDigits() {
		return floatPrecision();
	}

	public IntegerStruct floatPrecision() {
		final int precision = bigDecimal.precision();
		final BigInteger bigInteger = BigInteger.valueOf(precision);
		return new IntegerStruct(bigInteger);
	}

	/*
// NOTE: custom functions: 'single-float-bits'

(defconstant single-float-bias 126)	; Intel says 127
(defconstant single-float-exponent-byte (byte 8 23))
(defconstant single-float-significand-byte (byte 23 0))
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))
(defconstant single-float-trapping-nan-bit (ash 1 22))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

;;; INTEGER-DECODE-SINGLE-DENORM  --  Internal
;;;
;;;    Handle the denormalized case of INTEGER-DECODE-FLOAT for SINGLE-FLOAT.
;;;
(defun integer-decode-single-denorm (x)
  (let* ((bits (single-float-bits (abs x)))
		 (sig (ash (ldb vm:single-float-significand-byte bits) 1))
		 (extra-bias 0))
    (loop
      (unless (zerop (logand sig vm:single-float-hidden-bit))
		(return))
      (setq sig (ash sig 1))
      (incf extra-bias))
    (values sig
	        (- (- vm:single-float-bias) vm:single-float-digits extra-bias)
	        (if (minusp (float-sign x)) -1 1))))

;;; INTEGER-DECODE-SINGLE-FLOAT  --  Internal
;;;
;;;    Handle the single-float case of INTEGER-DECODE-FLOAT.  If an infinity or
;;; NAN, error.  If a denorm, call i-d-s-DENORM to handle it.
;;;
(defun integer-decode-single-float (x)
  (let* ((bits (single-float-bits (abs x)))
		 (exp (ldb vm:single-float-exponent-byte bits))
		 (sig (ldb vm:single-float-significand-byte bits))
		 (sign (if (minusp (float-sign x)) -1 1))
		 (biased (- exp vm:single-float-bias vm:single-float-digits)))
    (unless (<= exp vm:single-float-normal-exponent-max)
      (error (intl:gettext "Can't decode NAN or infinity: ~S.") x))
    (cond ((and (zerop exp) (zerop sig))
		   (values 0 biased sign))
		  ((< exp vm:single-float-normal-exponent-min)
		   (integer-decode-single-denorm x))
		  (t
		   (values (logior sig vm:single-float-hidden-bit) biased sign)))))
	 */
	public DecodeFloatResult integerDecodeFloat() {

		final long bits = Double.doubleToRawLongBits(doubleValue());
		final int signInt = ((bits >> 63) == 0) ? 1 : -1;
		final int exponentInt = (int) ((bits >> 52) & 0x7ffL);
		final long m;
		if (exponentInt == 0) {
			m = (bits & 0xfffffffffffffL) << 1;
		} else {
			m = (bits & 0xfffffffffffffL) | 0x10000000000000L;
		}
		IntegerStruct significand = new IntegerStruct(BigInteger.valueOf(m));
		IntegerStruct exponent = new IntegerStruct(BigInteger.valueOf(exponentInt - 1075));
		IntegerStruct sign = new IntegerStruct(BigInteger.valueOf(signInt));
		return new DecodeFloatResult(significand, exponent, sign);
	}

	// Strategy Implementations

	private static class FloatAddStrategy extends RealAddStrategy<FloatStruct> {

		private static final FloatAddStrategy INSTANCE = new FloatAddStrategy();

		@Override
		public RealStruct add(final FloatStruct number1, final IntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return new FloatStruct(add);
		}

		@Override
		public RealStruct add(final FloatStruct number1, final RatioStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return new FloatStruct(add);
		}
	}

	private static class FloatSubtractStrategy extends RealSubtractStrategy<FloatStruct> {

		private static final FloatSubtractStrategy INSTANCE = new FloatSubtractStrategy();

		@Override
		public RealStruct subtract(final FloatStruct number1, final IntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return new FloatStruct(subtract);
		}

		@Override
		public RealStruct subtract(final FloatStruct number1, final RatioStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return new FloatStruct(subtract);
		}
	}

	private static class FloatMultiplyStrategy extends RealMultiplyStrategy<FloatStruct> {

		private static final FloatMultiplyStrategy INSTANCE = new FloatMultiplyStrategy();

		@Override
		public RealStruct multiply(final FloatStruct number1, final IntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return new FloatStruct(multiply);
		}

		@Override
		public RealStruct multiply(final FloatStruct number1, final RatioStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return new FloatStruct(multiply);
		}
	}

	private static class FloatDivideStrategy extends RealDivideStrategy<FloatStruct> {

		private static final FloatDivideStrategy INSTANCE = new FloatDivideStrategy();

		@Override
		public RealStruct divide(final FloatStruct number1, final IntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return new FloatStruct(divide);
		}

		@Override
		public RealStruct divide(final FloatStruct number1, final RatioStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return new FloatStruct(divide);
		}
	}

	private static class FloatEqualToStrategy extends EqualToStrategy<FloatStruct> {

		private static final FloatEqualToStrategy INSTANCE = new FloatEqualToStrategy();

		@Override
		public boolean equalTo(final FloatStruct number1, final IntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) == 0;
		}

		@Override
		public boolean equalTo(final FloatStruct number1, final FloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) == 0;
		}

		@Override
		public boolean equalTo(final FloatStruct number1, final RatioStruct number2) {
			final RationalStruct rational1 = number1.rational();
			return rational1.isEqualTo(number2);
		}

		@Override
		public boolean equalTo(final FloatStruct number1, final ComplexStruct number2) {
			return number2.isEqualTo(number1);
		}
	}

	private static class FloatLessThanStrategy extends LessThanStrategy<FloatStruct> {

		private static final FloatLessThanStrategy INSTANCE = new FloatLessThanStrategy();

		@Override
		public boolean lessThan(final FloatStruct real1, final IntegerStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) < 0;
		}

		@Override
		public boolean lessThan(final FloatStruct real1, final FloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) < 0;
		}

		@Override
		public boolean lessThan(final FloatStruct real1, final RatioStruct real2) {
			final RationalStruct rational1 = real1.rational();
			return rational1.isLessThan(real2);
		}
	}

	private static class FloatGreaterThanStrategy extends GreaterThanStrategy<FloatStruct> {

		private static final FloatGreaterThanStrategy INSTANCE = new FloatGreaterThanStrategy();

		@Override
		public boolean greaterThan(final FloatStruct real1, final IntegerStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) > 0;
		}

		@Override
		public boolean greaterThan(final FloatStruct real1, final FloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) > 0;
		}

		@Override
		public boolean greaterThan(final FloatStruct real1, final RatioStruct real2) {
			final RationalStruct rational1 = real1.rational();
			return rational1.isGreaterThan(real2);
		}
	}

	private static class FloatLessThanOrEqualToStrategy extends LessThanOrEqualToStrategy<FloatStruct> {

		private static final FloatLessThanOrEqualToStrategy INSTANCE = new FloatLessThanOrEqualToStrategy();

		@Override
		public boolean lessThanOrEqualTo(final FloatStruct real1, final IntegerStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final FloatStruct real1, final FloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final FloatStruct real1, final RatioStruct real2) {
			final RationalStruct rational1 = real1.rational();
			return rational1.isLessThanOrEqualTo(real2);
		}
	}

	private static class FloatGreaterThanOrEqualToStrategy extends GreaterThanOrEqualToStrategy<FloatStruct> {

		private static final FloatGreaterThanOrEqualToStrategy INSTANCE = new FloatGreaterThanOrEqualToStrategy();

		@Override
		public boolean greaterThanOrEqualTo(final FloatStruct real1, final IntegerStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final FloatStruct real1, final FloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final FloatStruct real1, final RatioStruct real2) {
			final RationalStruct rational1 = real1.rational();
			return rational1.isGreaterThanOrEqualTo(real2);
		}
	}

	private static class FloatQuotientRemainderStrategy extends QuotientRemainderStrategy<FloatStruct> {

		private static final FloatQuotientRemainderStrategy INSTANCE = new FloatQuotientRemainderStrategy();

		@Override
		public QuotientRemainderResult quotientRemainder(final FloatStruct real, final IntegerStruct divisor,
		                                                 final RoundingMode roundingMode,
		                                                 final boolean isFloatResult) {
			return floatQuotientRemainder(real, divisor, roundingMode, isFloatResult);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final FloatStruct real, final RatioStruct divisor,
		                                                 final RoundingMode roundingMode,
		                                                 final boolean isFloatResult) {
			return floatQuotientRemainder(real, divisor, roundingMode, isFloatResult);
		}
	}

	// HashCode / Equals / ToString

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bigDecimal)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final FloatStruct rhs = (FloatStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigDecimal, rhs.bigDecimal)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bigDecimal)
		                                                                .toString();
	}
}

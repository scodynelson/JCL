/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import jcl.util.NumberUtils;
import org.apfloat.Apfloat;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public interface FloatStruct extends RealStruct {

	/**
	 * Returns this FloatStruct as a {@code float} value.
	 *
	 * @return this FloatStruct as a {@code float} value
	 */
	float floatValue();

	/**
	 * Returns this FloatStruct as a {@code double} value.
	 *
	 * @return this FloatStruct as a {@code double} value
	 */
	double doubleValue();

	/**
	 * Returns a new {@link FloatingPointVisitor} with this FloatStruct to be used as the prototype for conversion.
	 *
	 * @return a new {@link FloatingPointVisitor} with this FloatStruct to be used as the prototype for conversion
	 */
	FloatingPointVisitor<?> floatingPointVisitor();

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign..
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	DecodeFloatResult decodeFloat();

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign. The
	 * difference between this method an {@link #decodeFloat()} is that the significand and sign will both be {@link
	 * IntegerStruct}s with a special weighting between the significand and exponent based on the scaling needed for
	 * the
	 * significand to produce an {@link IntegerStruct}.
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	DecodeFloatResult integerDecodeFloat();

	/**
	 * Returns (* float (expt (float b float) scale)), where b is the radix of the floating-point representation.
	 *
	 * @param scale
	 * 		the
	 *
	 * @return this FloatStruct scaled to the provided scale value
	 */
	default NumberStruct scaleFloat(final IntegerStruct scale) {
		final IntegerStruct radix = floatRadix();
		final NumberStruct expt = radix.expt(scale);
		return multiply(expt);
	}

	/**
	 * Returns the number of radix b digits used in the representation of this FloatStruct.
	 *
	 * @return the number of radix b digits used in the representation of this FloatStruct
	 */
	default IntegerStruct floatDigits() {
		return floatPrecision();
	}

	/**
	 * Returns the number of significant radix b digits present in this FloatStruct.
	 *
	 * @return the number of significant radix b digits present in this FloatStruct
	 */
	IntegerStruct floatPrecision();

	/**
	 * The radix of the FloatStruct.
	 *
	 * @return the radix of the FloatStruct
	 */
	default IntegerStruct floatRadix() {
		return IntegerStruct.TWO;
	}

	/**
	 * Returns either a {@code 1} or a {@code -1} value based on the sign of the FloatStruct.
	 *
	 * @return a {@code 1} or a {@code -1} value based on the sign of the FloatStruct
	 */
	FloatStruct floatSign();

	/**
	 * Returns a number z such that z and this FloatStruct have the same sign and also such that z and float2 have the
	 * same absolute value.
	 *
	 * @param float2
	 * 		the value of the resulting FloatStruct
	 *
	 * @return a number z such that z and this FloatStruct have the same sign and also such that z and float2 have the
	 * same absolute value
	 */
	default FloatStruct floatSign(final FloatStruct float2) {
		if (minusp()) {
			if (float2.minusp()) {
				return float2;
			} else {
				return (FloatStruct) float2.negation();
			}
		} else {
			return (FloatStruct) float2.abs();
		}
	}

	/*
		RealStruct
	 */

	@Override
	default FloatStruct floatingPoint() {
		return this;
	}

	@Override
	default QuotientRemainderResult floor(final RealStruct.QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.floor(this);
	}

	@Override
	default QuotientRemainderResult ffloor(final RealStruct.QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ffloor(this);
	}

	@Override
	default QuotientRemainderResult ceiling(final RealStruct.QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ceiling(this);
	}

	@Override
	default QuotientRemainderResult fceiling(final RealStruct.QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fceiling(this);
	}

	@Override
	default QuotientRemainderResult round(final RealStruct.QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.round(this);
	}

	@Override
	default QuotientRemainderResult fround(final RealStruct.QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fround(this);
	}

	@Override
	default QuotientRemainderResult truncate(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.truncate(this);
	}

	@Override
	default QuotientRemainderResult ftruncate(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ftruncate(this);
	}

	@Override
	default RealStruct.QuotientRemainderVisitor<?> quotientRemainderVisitor() {
		return new FloatQuotientRemainderVisitor(this);
	}

	// Visitor Implementations

	/**
	 * {@link RealStruct.RealEqualToVisitor} for computing numeric '=' equality results for {@link FloatStruct}s.
	 */
	abstract class FloatEqualToVisitor<R extends FloatStruct> extends RealStruct.RealEqualToVisitor<R> {

		/**
		 * Package private constructor to make a new instance of an FloatEqualToVisitor with the provided {@link
		 * FloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		FloatEqualToVisitor(final R number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final IntIntegerStruct number2) {
			final RationalStruct number1Rational = number1.rational();
			return number1Rational.isEqualTo(number2);
		}

		@Override
		public boolean equalTo(final LongIntegerStruct number2) {
			final RationalStruct number1Rational = number1.rational();
			return number1Rational.isEqualTo(number2);
		}

		@Override
		public boolean equalTo(final BigIntegerStruct number2) {
			final RationalStruct number1Rational = number1.rational();
			return number1Rational.isEqualTo(number2);
		}

		@Override
		public boolean equalTo(final RatioStruct number2) {
			final RationalStruct number1Rational = number1.rational();
			return number1Rational.isEqualTo(number2);
		}
	}

	/**
	 * {@link RealStruct.LessThanVisitor} for computing numeric {@literal '<'} equality results for {@link
	 * FloatStruct}s.
	 */
	abstract class FloatLessThanVisitor<R extends FloatStruct> extends RealStruct.LessThanVisitor<R> {

		/**
		 * Package private constructor to make a new instance of an FloatLessThanVisitor with the provided {@link
		 * FloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		FloatLessThanVisitor(final R real1) {
			super(real1);
		}

		@Override
		public boolean lessThan(final IntIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isLessThan(real2);
		}

		@Override
		public boolean lessThan(final LongIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isLessThan(real2);
		}

		@Override
		public boolean lessThan(final BigIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isLessThan(real2);
		}

		@Override
		public boolean lessThan(final RatioStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isLessThan(real2);
		}
	}

	/**
	 * {@link RealStruct.GreaterThanVisitor} for computing numeric {@literal '>'} equality results for {@link
	 * FloatStruct}s.
	 */
	abstract class FloatGreaterThanVisitor<R extends FloatStruct> extends RealStruct.GreaterThanVisitor<R> {

		/**
		 * Package private constructor to make a new instance of an FloatGreaterThanVisitor with the provided {@link
		 * FloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		FloatGreaterThanVisitor(final R real1) {
			super(real1);
		}

		@Override
		public boolean greaterThan(final IntIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isGreaterThan(real2);
		}

		@Override
		public boolean greaterThan(final LongIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isGreaterThan(real2);
		}

		@Override
		public boolean greaterThan(final BigIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isGreaterThan(real2);
		}

		@Override
		public boolean greaterThan(final RatioStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isGreaterThan(real2);
		}
	}

	/**
	 * {@link RealStruct.LessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality results for {@link
	 * FloatStruct}s.
	 */
	abstract class FloatLessThanOrEqualToVisitor<R extends FloatStruct> extends RealStruct.LessThanOrEqualToVisitor<R> {

		/**
		 * Package private constructor to make a new instance of an FloatLessThanOrEqualToVisitor with the provided
		 * {@link FloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		FloatLessThanOrEqualToVisitor(final R real1) {
			super(real1);
		}

		@Override
		public boolean lessThanOrEqualTo(final IntIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isLessThanOrEqualTo(real2);
		}

		@Override
		public boolean lessThanOrEqualTo(final LongIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isLessThanOrEqualTo(real2);
		}

		@Override
		public boolean lessThanOrEqualTo(final BigIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isLessThanOrEqualTo(real2);
		}

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isLessThanOrEqualTo(real2);
		}
	}

	/**
	 * {@link RealStruct.GreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality results for {@link
	 * FloatStruct}s.
	 */
	abstract class FloatGreaterThanOrEqualToVisitor<R extends FloatStruct> extends RealStruct.GreaterThanOrEqualToVisitor<R> {

		/**
		 * Package private constructor to make a new instance of an FloatGreaterThanOrEqualToVisitor with the
		 * provided {@link FloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		FloatGreaterThanOrEqualToVisitor(final R real1) {
			super(real1);
		}

		@Override
		public boolean greaterThanOrEqualTo(final IntIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isGreaterThanOrEqualTo(real2);
		}

		@Override
		public boolean greaterThanOrEqualTo(final LongIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isGreaterThanOrEqualTo(real2);
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigIntegerStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isGreaterThanOrEqualTo(real2);
		}

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			final RationalStruct real1Rational = real1.rational();
			return real1Rational.isGreaterThanOrEqualTo(real2);
		}
	}

	/**
	 * {@link RealStruct.QuotientRemainderVisitor} for computing quotient and remainder results for {@link
	 * FloatStruct}s.
	 */
	final class FloatQuotientRemainderVisitor extends RealStruct.QuotientRemainderVisitor<FloatStruct> {

		/**
		 * Private constructor to make a new instance of an FloatQuotientRemainderVisitor with the provided {@link
		 * FloatStruct}.
		 *
		 * @param real
		 * 		the real argument in the computational quotient and remainder operation
		 */
		private FloatQuotientRemainderVisitor(final FloatStruct real) {
			super(real);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final IntegerStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			final FloatStruct divisorFloat = divisor.floatingPoint();
			return quotientRemainder(divisorFloat, roundingMode, isQuotientFloat);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final FloatStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			final BigDecimal realBD = NumberUtils.bigDecimalValue(real.doubleValue());
			final BigDecimal divisorBD = NumberUtils.bigDecimalValue(divisor.doubleValue());

			final BigDecimal quotient = realBD.divide(divisorBD, 0, roundingMode);
			final BigDecimal remainder = realBD.subtract(quotient.multiply(divisorBD));

			final RealStruct quotientReal;
			if (isQuotientFloat) {
				quotientReal = getFloatQuotient(real, divisor, quotient);
			} else {
				final BigInteger quotientBigInteger = quotient.toBigInteger();
				quotientReal = IntegerStruct.valueOf(quotientBigInteger);
			}

			final FloatStruct remainderFloat = DoubleFloatStruct.valueOf(remainder.doubleValue());
			return new QuotientRemainderResult(quotientReal, remainderFloat);
		}

		private static FloatStruct getFloatQuotient(final FloatStruct real, final FloatStruct divisor,
		                                            final BigDecimal quotient) {
			final FloatStruct floatQuotient;
			if (BigDecimal.ZERO.compareTo(quotient) == 0) {
				if (real.minusp()) {
					if (divisor.minusp()) {
						floatQuotient = DoubleFloatStruct.ZERO;
					} else {
						floatQuotient = DoubleFloatStruct.MINUS_ZERO;
					}
				} else if (divisor.minusp()) {
					floatQuotient = DoubleFloatStruct.MINUS_ZERO;
				} else {
					floatQuotient = DoubleFloatStruct.ZERO;
				}
			} else {
				floatQuotient = DoubleFloatStruct.valueOf(quotient.doubleValue());
			}
			return floatQuotient;
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final RatioStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			final FloatStruct divisorFloat = divisor.floatingPoint();
			return quotientRemainder(divisorFloat, roundingMode, isQuotientFloat);
		}
	}

	/*
		Deprecated
	 */

	@Deprecated
	static FloatStruct valueOf(final Apfloat apfloat) {
		return SingleFloatStruct.valueOf(apfloat.floatValue());
	}

	@Deprecated
	BigDecimal bigDecimalValue();
}

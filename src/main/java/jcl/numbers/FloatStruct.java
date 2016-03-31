/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.RoundingMode;

import org.apfloat.Apfloat;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public interface FloatStruct extends RealStruct {

	float floatValue();

	double doubleValue();

	BigDecimal bigDecimalValue();

	FloatingPointVisitor<?> floatingPointVisitor();

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign. The
	 * calculation for these values are based on the decoding for Java {@link Double} values from the algorithm defined
	 * in {@link Double#longBitsToDouble}.
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	DecodeFloatResult decodeFloat();

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign. The
	 * calculation for these values are based on the decoding for Java {@link Double} values from the algorithm defined
	 * in {@link Double#longBitsToDouble}. The difference between this method an {@link #decodeFloat()} is that the
	 * significand and sign will both be {@link IntegerStruct}s with a special weighting between the significand and
	 * exponent based on the scaling needed for the significand to produce an {@link IntegerStruct}.
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	DecodeFloatResult integerDecodeFloat();

	default NumberStruct scaleFloat(final IntegerStruct scale) {
		final IntegerStruct radix = floatRadix();
		final NumberStruct expt = radix.expt(scale);
		return multiply(expt);
	}

	default IntegerStruct floatDigits() {
		return floatPrecision();
	}

	IntegerStruct floatPrecision();

	default IntegerStruct floatRadix() {
		return IntegerStruct.TWO;
	}

	FloatStruct floatSign();

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
	 * {@link FloatQuotientRemainderVisitor} for computing quotient and remainder results for {@link FloatStruct}s.
	 */
	final class FloatQuotientRemainderVisitor extends RealStruct.QuotientRemainderVisitor<FloatStruct> {

		/**
		 * Private constructor to make a new instance of an FloatQuotientRemainderVisitor with the provided {@link
		 * FloatStruct}.
		 *
		 * @param real
		 * 		the real argument in the computational quotient and remainder operation
		 */
		FloatQuotientRemainderVisitor(final FloatStruct real) {
			super(real);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final IntegerStruct divisor, final RoundingMode roundingMode, final boolean isQuotientFloat) {
			return floatQuotientRemainder(divisor, roundingMode, isQuotientFloat);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final FloatStruct divisor, final RoundingMode roundingMode, final boolean isQuotientFloat) {
			return super.quotientRemainder(divisor, roundingMode, isQuotientFloat);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final RatioStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			return floatQuotientRemainder(divisor, roundingMode, isQuotientFloat);
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
	default BigDecimal getBigDecimal() {
		return bigDecimalValue();
	}
}

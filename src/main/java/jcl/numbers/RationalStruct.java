/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import jcl.util.NumberUtils;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link RationalStruct} is the object representation of a Lisp 'rational' type.
 */
public interface RationalStruct extends RealStruct {

	/**
	 * Returns numerator of this RationalStruct as an {@link IntegerStruct}.
	 *
	 * @return the numerator value
	 */
	IntegerStruct numerator();

	/**
	 * Returns denominator of this RationalStruct as an {@link IntegerStruct}.
	 *
	 * @return the denominator value
	 */
	IntegerStruct denominator();

	/**
	 * Returns a new RationalStruct with the provided {@code int} as a numerator. Because the default denominator value
	 * is {@code 1}, the result will be an {@link IntegerStruct}.
	 *
	 * @param numerator
	 * 		the {@code int} numerator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided {@code int} as a numerator
	 */
	static RationalStruct valueOf(final Integer numerator) {
		return IntegerStruct.valueOf(numerator);
	}

	/**
	 * Returns a new RationalStruct with the numerator and denominator values. If the denominator value is {@code 1},
	 * the result will be an {@link IntegerStruct}.
	 *
	 * @param numerator
	 * 		the {@code int} numerator of the new RationalStruct
	 * @param denominator
	 * 		the {@code int} denominator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided numerator and denominator values
	 */
	static RationalStruct valueOf(final Integer numerator, final Integer denominator) {
		if (denominator == 1) {
			return IntegerStruct.valueOf(numerator);
		}
		final BigFraction bigFraction = new BigFraction(numerator, denominator);
		return RatioStruct.valueOf(bigFraction);
	}

	/**
	 * Returns a new RationalStruct with the provided {@code long} as a numerator. Because the default denominator
	 * value is {@code 1}, the result will be an {@link IntegerStruct}.
	 *
	 * @param numerator
	 * 		the {@code long} numerator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided {@code long} as a numerator
	 */
	static RationalStruct valueOf(final Long numerator) {
		return IntegerStruct.valueOf(numerator);
	}

	/**
	 * Returns a new RationalStruct with the numerator and denominator values. If the denominator value is {@code 1},
	 * the result will be an {@link IntegerStruct}.
	 *
	 * @param numerator
	 * 		the {@code long} numerator of the new RationalStruct
	 * @param denominator
	 * 		the {@code long} denominator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided numerator and denominator values
	 */
	static RationalStruct valueOf(final Long numerator, final Long denominator) {
		if (denominator == 1L) {
			return IntegerStruct.valueOf(numerator);
		}
		final BigFraction bigFraction = new BigFraction(numerator, denominator);
		return RatioStruct.valueOf(bigFraction);
	}

	/**
	 * Returns a new RationalStruct with the provided {@link BigInteger} as a numerator. Because the default
	 * denominator
	 * value is {@code 1}, the result will be an {@link IntegerStruct}.
	 *
	 * @param numerator
	 * 		the {@link BigInteger} numerator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided {@link BigInteger} as a numerator
	 */
	static RationalStruct valueOf(final BigInteger numerator) {
		return IntegerStruct.valueOf(numerator);
	}

	/**
	 * Returns a new RationalStruct with the numerator and denominator values. If the denominator value is {@code 1},
	 * the result will be an {@link IntegerStruct}.
	 *
	 * @param numerator
	 * 		the {@link BigInteger} numerator of the new RationalStruct
	 * @param denominator
	 * 		the {@link BigInteger} denominator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided numerator and denominator values
	 */
	static RationalStruct valueOf(final BigInteger numerator, final BigInteger denominator) {
		if (BigInteger.ONE.compareTo(denominator) == 0) {
			return IntegerStruct.valueOf(numerator);
		}
		final BigFraction bigFraction = new BigFraction(numerator, denominator);
		return RatioStruct.valueOf(bigFraction);
	}

	/**
	 * Returns a new RationalStruct representing the provided {@link BigFraction}. If the {@link
	 * BigFraction#denominator} value is equal to {@link BigInteger#ONE}, the result will be an {@link IntegerStruct}.
	 *
	 * @param bigFraction
	 * 		the {@link BigFraction} used to create the resulting RationalStruct
	 *
	 * @return a new RationalStruct representing the provided {@link BigFraction}
	 */
	static RationalStruct valueOf(final BigFraction bigFraction) {
		final BigInteger denominator = bigFraction.getDenominator();
		if (denominator.compareTo(BigInteger.ONE) == 0) {
			final BigInteger numerator = bigFraction.getNumerator();
			return IntegerStruct.valueOf(numerator);
		}
		return RatioStruct.valueOf(bigFraction);
	}

	/*
		RealStruct
	 */

	@Override
	default RationalStruct rational() {
		return this;
	}

	/*
		NumberStruct
	 */

	@Override
	default NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			if (power instanceof IntegerStruct) {
				return IntegerStruct.ONE;
			}
			return SingleFloatStruct.ONE;
		}

		if (zerop() || isEqualTo(IntegerStruct.ONE)) {
			return this;
		}

		final ExptVisitor<?> exptVisitor = exptVisitor();
		return power.expt(exptVisitor);
	}

	@Override
	default NumberStruct signum() {
		if (zerop()) {
			return this;
		} else if (plusp()) {
			return IntegerStruct.ONE;
		} else {
			return IntegerStruct.MINUS_ONE;
		}
	}

	@Override
	default RealStruct imagPart() {
		return IntegerStruct.ZERO;
	}

	// Visitor Implementations

	/**
	 * {@link RealStruct.RealEqualToVisitor} for computing numeric '=' equality results for {@link RationalStruct}s.
	 */
	abstract class RationalEqualToVisitor<R extends RationalStruct> extends RealStruct.RealEqualToVisitor<R> {

		/**
		 * Package private constructor to make a new instance of an RationalEqualToVisitor with the provided {@link
		 * RationalStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		RationalEqualToVisitor(final R number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final SingleFloatStruct number2) {
			final RationalStruct number2Rational = number2.rational();
			return number1.isEqualTo(number2Rational);
		}

		@Override
		public boolean equalTo(final DoubleFloatStruct number2) {
			final RationalStruct number2Rational = number2.rational();
			return number1.isEqualTo(number2Rational);
		}
	}

	/**
	 * {@link RealStruct.LessThanVisitor} for computing numeric {@literal '<'} equality results for {@link
	 * RationalStruct}s.
	 */
	abstract class RationalLessThanVisitor<R extends RationalStruct> extends RealStruct.LessThanVisitor<R> {

		/**
		 * Package private constructor to make a new instance of an RationalLessThanVisitor with the provided {@link
		 * RationalStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		RationalLessThanVisitor(final R real1) {
			super(real1);
		}

		@Override
		public boolean lessThan(final SingleFloatStruct real2) {
			final RationalStruct number2Rational = real2.rational();
			return real1.isLessThan(number2Rational);
		}

		@Override
		public boolean lessThan(final DoubleFloatStruct real2) {
			final RationalStruct number2Rational = real2.rational();
			return real1.isLessThan(number2Rational);
		}
	}

	/**
	 * {@link RealStruct.GreaterThanVisitor} for computing numeric {@literal '>'} equality results for {@link
	 * RationalStruct}s.
	 */
	abstract class RationalGreaterThanVisitor<R extends RationalStruct> extends RealStruct.GreaterThanVisitor<R> {

		/**
		 * Package private constructor to make a new instance of an RationalGreaterThanVisitor with the provided {@link
		 * RationalStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		RationalGreaterThanVisitor(final R real1) {
			super(real1);
		}

		@Override
		public boolean greaterThan(final SingleFloatStruct real2) {
			final RationalStruct number2Rational = real2.rational();
			return real1.isGreaterThan(number2Rational);
		}

		@Override
		public boolean greaterThan(final DoubleFloatStruct real2) {
			final RationalStruct number2Rational = real2.rational();
			return real1.isGreaterThan(number2Rational);
		}
	}

	/**
	 * {@link RealStruct.LessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality results for {@link
	 * RationalStruct}s.
	 */
	abstract class RationalLessThanOrEqualToVisitor<R extends RationalStruct> extends RealStruct.LessThanOrEqualToVisitor<R> {

		/**
		 * Package private constructor to make a new instance of an RationalLessThanOrEqualToVisitor with the provided
		 * {@link RationalStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		RationalLessThanOrEqualToVisitor(final R real1) {
			super(real1);
		}

		@Override
		public boolean lessThanOrEqualTo(final SingleFloatStruct real2) {
			final RationalStruct number2Rational = real2.rational();
			return real1.isLessThanOrEqualTo(number2Rational);
		}

		@Override
		public boolean lessThanOrEqualTo(final DoubleFloatStruct real2) {
			final RationalStruct number2Rational = real2.rational();
			return real1.isLessThanOrEqualTo(number2Rational);
		}
	}

	/**
	 * {@link RealStruct.GreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality results for {@link
	 * RationalStruct}s.
	 */
	abstract class RationalGreaterThanOrEqualToVisitor<R extends RationalStruct> extends RealStruct.GreaterThanOrEqualToVisitor<R> {

		/**
		 * Package private constructor to make a new instance of an RationalGreaterThanOrEqualToVisitor with the
		 * provided {@link RationalStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		RationalGreaterThanOrEqualToVisitor(final R real1) {
			super(real1);
		}

		@Override
		public boolean greaterThanOrEqualTo(final SingleFloatStruct real2) {
			final RationalStruct number2Rational = real2.rational();
			return real1.isGreaterThanOrEqualTo(number2Rational);
		}

		@Override
		public boolean greaterThanOrEqualTo(final DoubleFloatStruct real2) {
			final RationalStruct number2Rational = real2.rational();
			return real1.isGreaterThanOrEqualTo(number2Rational);
		}
	}

	/**
	 * {@link RealStruct.QuotientRemainderVisitor} for computing quotient and remainder results for {@link
	 * RationalStruct}s.
	 */
	abstract class RationalQuotientRemainderVisitor<S extends RationalStruct> extends RealStruct.QuotientRemainderVisitor<S> {

		/**
		 * Package private constructor to make a new instance of an RationalQuotientRemainderVisitor with the provided
		 * {@link RationalStruct}.
		 *
		 * @param real
		 * 		the real argument in the computational quotient and remainder operation
		 */
		RationalQuotientRemainderVisitor(final S real) {
			super(real);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final FloatStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			final FloatStruct realFloat = real.floatingPoint();
			final RealStruct.QuotientRemainderVisitor<?> quotientRemainderVisitor = realFloat.quotientRemainderVisitor();
			return quotientRemainderVisitor.quotientRemainder(divisor, roundingMode, isQuotientFloat);
		}

		QuotientRemainderResult getQuotientRemainderResult(final BigInteger realNumerator, final BigInteger realDenominator,
		                                                   final BigInteger divisorNumerator, final BigInteger divisorDenominator,
		                                                   final RoundingMode roundingMode, final boolean isQuotientFloat) {
			// Invert and multiply
			final BigInteger multipliedNumerator = realNumerator.multiply(divisorDenominator);
			final BigInteger multipliedDenominator = realDenominator.multiply(divisorNumerator);
			final BigDecimal multipliedNumeratorBD = NumberUtils.bigDecimalValue(multipliedNumerator);
			final BigDecimal multipliedDenominatorBD = NumberUtils.bigDecimalValue(multipliedDenominator);

			// Divide to get quotient
			final BigDecimal quotient = multipliedNumeratorBD.divide(multipliedDenominatorBD, 0, roundingMode);
			final BigInteger quotientBI = quotient.toBigInteger();

			final RealStruct quotientReal;
			if (isQuotientFloat) {
				quotientReal = SingleFloatStruct.valueOf(quotient.doubleValue());
			} else {
				quotientReal = IntegerStruct.valueOf(quotientBI);
			}

			// Multiply divisor by quotient
			final BigInteger multiply = divisorNumerator.multiply(quotientBI);
			final RationalStruct product = RationalStruct.valueOf(multiply, divisorDenominator);

			// Subtract to get remainder: (Rational - Rational) produces Rational
			final RationalStruct remainder = (RationalStruct) real.subtract(product);

			return new QuotientRemainderResult(quotientReal, remainder);
		}
	}
}

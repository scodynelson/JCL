/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import jcl.conditions.exceptions.DivisionByZeroException;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link RationalStruct} is the object representation of a Lisp 'rational' type.
 */
public interface RationalStruct extends RealStruct {

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link BigFraction#numerator} from {@link #bigFraction} as a new {@link IntegerStruct}.
	 */
	IntegerStruct numerator();

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link BigFraction#denominator} from {@link #bigFraction} as a new {@link IntegerStruct}.
	 */
	IntegerStruct denominator();

	static RationalStruct valueOf(final BigFraction bigFraction) {
		final BigInteger denominator = bigFraction.getDenominator();
		if (denominator.compareTo(BigInteger.ONE) == 0) {
			final BigInteger numerator = bigFraction.getNumerator();
			return IntegerStruct.valueOf(numerator);
		}
		return RatioStruct.valueOf(bigFraction);
	}

	static RationalStruct valueOf(final int numerator) {
		return IntegerStruct.valueOf(numerator);
	}

	static RationalStruct valueOf(final int numerator, final int denominator) {
		if (denominator == 1) {
			return IntegerStruct.valueOf(numerator);
		}
		final BigFraction bigFraction = new BigFraction(numerator, denominator);
		return RatioStruct.valueOf(bigFraction);
	}

	static RationalStruct valueOf(final long numerator) {
		return IntegerStruct.valueOf(numerator);
	}

	static RationalStruct valueOf(final long numerator, final long denominator) {
		if (denominator == 1L) {
			return IntegerStruct.valueOf(numerator);
		}
		final BigFraction bigFraction = new BigFraction(numerator, denominator);
		return RatioStruct.valueOf(bigFraction);
	}

	static RationalStruct valueOf(final BigInteger numerator) {
		return IntegerStruct.valueOf(numerator);
	}

	static RationalStruct valueOf(final BigInteger numerator, final BigInteger denominator) {
		if (BigInteger.ONE.compareTo(denominator) == 0) {
			return IntegerStruct.valueOf(numerator);
		}
		final BigFraction bigFraction = new BigFraction(numerator, denominator);
		return RatioStruct.valueOf(bigFraction);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link IntegerStruct#ONE} as the imaginary part of IntegerStructs is always '1'.
	 */
	@Override
	default RealStruct imagPart() {
		return IntegerStruct.ZERO;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the whether or not the numerical value of this RatioStruct is zero, positive, or negative,
	 * returning {@code this}, {@link IntegerStruct#ONE}, or {@link IntegerStruct#MINUS_ONE} respectively.
	 */
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

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@code this} as any RationalStruct is already in rational form.
	 */
	@Override
	default RationalStruct rational() {
		return this;
	}

	static RationalStruct makeRational(final BigFraction bigFraction) {
		final BigFraction reduced = bigFraction.reduce();
		final BigInteger numerator = reduced.getNumerator();
		final BigInteger denominator = reduced.getDenominator();
		return makeRational(numerator, denominator);
	}

	static RationalStruct makeRational(final BigInteger numerator, final BigInteger denominator) {
		if (BigInteger.ZERO.compareTo(denominator) == 0) {
			// TODO: what do we pass to this exception???
			throw new DivisionByZeroException("Division By Zero");
		}

		BigInteger realNumerator = numerator;
		BigInteger realDenominator = denominator;

		// Possibly flip Numerator and Denominator signs
		if (realDenominator.signum() < 0) {
			realNumerator = realNumerator.negate();
			realDenominator = realDenominator.negate();
		}

		// Reduce Numerator and Denominator
		final BigInteger gcd = realNumerator.gcd(realDenominator);
		if (BigInteger.ONE.compareTo(gcd) != 0) {
			realNumerator = realNumerator.divide(gcd);
			realDenominator = realDenominator.divide(gcd);
		}

		// If reduced Denominator is '1', return an Integer; otherwise, return the Ratio with the Numerator and Denominator
		if (BigInteger.ONE.compareTo(realDenominator) == 0) {
			return IntegerStruct.valueOf(realNumerator);
		} else {
			return valueOf(realNumerator, realDenominator);
		}
	}

	// Visitor Implementations

	class RationalQuotientRemainderVisitor<S extends RationalStruct> extends RealStruct.QuotientRemainderVisitor<S> {

		RationalQuotientRemainderVisitor(final S real) {
			super(real);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final RatioStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			return ratioQuotientRemainder(divisor, roundingMode, isQuotientFloat);
		}

		QuotientRemainderResult ratioQuotientRemainder(final RationalStruct divisor, final RoundingMode roundingMode,
		                                               final boolean isFloatResult) {
			final IntegerStruct rationalNumerator = real.numerator();
			final BigInteger rationalNumeratorBigInteger = rationalNumerator.bigIntegerValue();
			final IntegerStruct rationalDenominator = real.denominator();
			final BigInteger rationalDenominatorBigInteger = rationalDenominator.bigIntegerValue();

			final IntegerStruct divisorNumerator = divisor.numerator();
			final BigInteger divisorNumeratorBigInteger = divisorNumerator.bigIntegerValue();
			final IntegerStruct divisorDenominator = divisor.denominator();
			final BigInteger divisorDenominatorBigInteger = divisorDenominator.bigIntegerValue();

			// Invert and multiply
			final BigInteger multipliedNumerator = rationalNumeratorBigInteger.multiply(divisorDenominatorBigInteger);
			final BigInteger multipliedDenominator = rationalDenominatorBigInteger.multiply(divisorNumeratorBigInteger);

			// Divide to get quotient
			final BigDecimal multipliedNumeratorBigDecimal = new BigDecimal(multipliedNumerator);
			final BigDecimal multipliedDenominatorBigDecimal = new BigDecimal(multipliedDenominator);
			final BigDecimal quotient = multipliedNumeratorBigDecimal.divide(multipliedDenominatorBigDecimal, 0, roundingMode);
			final BigInteger quotientBigInteger = quotient.toBigInteger();

			final RealStruct quotientReal;
			if (isFloatResult) {
				quotientReal = FloatStruct.valueOf(quotient);
			} else {
				quotientReal = IntegerStruct.valueOf(quotientBigInteger);
			}

			// Multiply divisor by quotient
			final BigInteger multiply = divisorNumeratorBigInteger.multiply(quotientBigInteger);
			final RationalStruct product = makeRational(multiply, divisorDenominatorBigInteger);

			// Subtract to get remainder: (Rational - Rational) produces Rational
			final NumberStruct remainder = real.subtract(product);

			return new QuotientRemainderResult(quotientReal, (RealStruct) remainder);
		}
	}
}

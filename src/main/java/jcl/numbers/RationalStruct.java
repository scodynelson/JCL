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

	IntegerStruct numerator();

	IntegerStruct denominator();

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
		final BigInteger numerator = bigFraction.getNumerator();
		final BigInteger denominator = bigFraction.getDenominator();
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
			return new RatioStruct(realNumerator, realDenominator);
		}
	}

	// Visitor Implementations

	abstract class RationalQuotientRemainderVisitor<S extends RationalStruct> extends RealStruct.QuotientRemainderVisitor<S> {

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
			final BigInteger rationalNumeratorBigInteger = rationalNumerator.getBigInteger();
			final IntegerStruct rationalDenominator = real.denominator();
			final BigInteger rationalDenominatorBigInteger = rationalDenominator.getBigInteger();

			final IntegerStruct divisorNumerator = divisor.numerator();
			final BigInteger divisorNumeratorBigInteger = divisorNumerator.getBigInteger();
			final IntegerStruct divisorDenominator = divisor.denominator();
			final BigInteger divisorDenominatorBigInteger = divisorDenominator.getBigInteger();

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

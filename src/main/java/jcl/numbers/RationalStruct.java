/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.List;

import jcl.LispStruct;
import jcl.conditions.exceptions.DivisionByZeroException;
import jcl.types.RationalType;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link RationalStruct} is the object representation of a Lisp 'rational' type.
 */
public abstract class RationalStruct extends RealStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3775544213011392520L;

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected RationalStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(RationalType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the rational object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected RationalStruct(final RationalType type,
	                         final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	public abstract IntegerStruct numerator();

	public abstract IntegerStruct denominator();

	@Override
	public FloatStruct coerceRealToFloat() {
		final BigDecimal bigDecimal = bigDecimalValue();
		return new FloatStruct(bigDecimal);
	}

	protected static RationalStruct makeRational(final BigFraction bigFraction) {
		final BigInteger numerator = bigFraction.getNumerator();
		final BigInteger denominator = bigFraction.getDenominator();
		return makeRational(numerator, denominator);
	}

	protected static RationalStruct makeRational(final BigInteger numerator, final BigInteger denominator) {
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
		if (!gcd.equals(BigInteger.ONE)) {
			realNumerator = realNumerator.divide(gcd);
			realDenominator = realDenominator.divide(gcd);
		}

		// If reduced Denominator is '1', return an Integer; otherwise, return the Ratio with the Numerator and Denominator
		if (realDenominator.equals(BigInteger.ONE)) {
			return new IntegerStruct(realNumerator);
		} else {
			return new RatioStruct(realNumerator, realDenominator);
		}
	}

	// Strategy Implementations

	protected abstract static class RationalEqualToStrategy<S extends RationalStruct> extends RealEqualToStrategy<S> {

		@Override
		public boolean equalTo(final S number1, final FloatStruct number2) {
			final RationalStruct rational2 = number2.rational();
			return number1.isEqualTo(rational2);
		}
	}

	protected abstract static class RationalQuotientRemainderStrategy<S extends RationalStruct> extends QuotientRemainderStrategy<S> {

		@Override
		public QuotientRemainderResult quotientRemainder(final S real, final RatioStruct divisor,
		                                                 final RoundingMode roundingMode,
		                                                 final boolean isFloatResult) {
			return ratioQuotientRemainder(real, divisor, roundingMode, isFloatResult);
		}

		protected static QuotientRemainderResult ratioQuotientRemainder(final RationalStruct rational, final RationalStruct divisor,
		                                                                final RoundingMode roundingMode, final boolean isFloatResult) {
			final IntegerStruct rationalNumerator = rational.numerator();
			final BigInteger rationalNumeratorBigInteger = rationalNumerator.getBigInteger();
			final IntegerStruct rationalDenominator = rational.denominator();
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
				quotientReal = new FloatStruct(quotient);
			} else {
				quotientReal = new IntegerStruct(quotientBigInteger);
			}

			// Multiply divisor by quotient
			final BigInteger multiply = divisorNumeratorBigInteger.multiply(quotientBigInteger);
			final RationalStruct product = makeRational(multiply, divisorDenominatorBigInteger);

			// Subtract to get remainder: (Rational - Rational) produces Rational
			final NumberStruct remainder = rational.subtract(product);

			return new QuotientRemainderResult(quotientReal, (RealStruct) remainder);
		}
	}
}

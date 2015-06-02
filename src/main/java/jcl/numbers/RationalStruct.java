/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
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

	protected abstract static class RationalFloorStrategy<S extends RationalStruct> extends FloorStrategy<S> {

		@Override
		public QuotientRemainderResult floor(final S real, final RatioStruct divisor) {
			return ratioFloor(real, divisor);
		}

		public QuotientRemainderResult ratioFloor(final RationalStruct rational, final RationalStruct divisor) {

			final IntegerStruct rationalNumerator = rational.numerator();
			final BigInteger rationalNumeratorBigInteger = rationalNumerator.getBigInteger();
			final IntegerStruct rationalDenominator = rational.denominator();
			final BigInteger rationalDenominatorBigInteger = rationalDenominator.getBigInteger();

			final IntegerStruct divisorNumerator = divisor.numerator();
			final BigInteger divisorNumeratorBigInteger = divisorNumerator.getBigInteger();
			final IntegerStruct divisorDenominator = divisor.denominator();
			final BigInteger divisorDenominatorBigInteger = divisorDenominator.getBigInteger();

			// Invert and multiply.
			final BigInteger num = rationalNumeratorBigInteger.multiply(divisorDenominatorBigInteger);
			final BigInteger den = rationalDenominatorBigInteger.multiply(divisorNumeratorBigInteger);

			final BigInteger quotient = num.divide(den);
			final IntegerStruct quotientInteger = new IntegerStruct(quotient);

			// Multiply quotient by divisor.
			final BigInteger multiply = quotient.multiply(divisorNumeratorBigInteger);
			final RationalStruct product = makeRational(multiply, divisorDenominatorBigInteger);

			// Subtract to get remainder. (Rational - Rational) produces a Rational
			final NumberStruct remainder = rational.subtract(product);

			return new QuotientRemainderResult(quotientInteger, (RealStruct) remainder);
		}
	}
}

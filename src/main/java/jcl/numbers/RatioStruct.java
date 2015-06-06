/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

import jcl.LispStruct;
import jcl.types.RatioType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public class RatioStruct extends RationalStruct {

	/**
	 * {@link RatioStruct} constant representing 0.
	 */
	public static final RatioStruct ZERO = new RatioStruct(BigFraction.ZERO);

	/**
	 * {@link RatioStruct} constant representing 1.
	 */
	public static final RatioStruct ONE = new RatioStruct(BigFraction.ONE);

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2468768422160538347L;

	/**
	 * The internal {@link BigFraction} containing the ratio contents.
	 */
	private final BigFraction bigFraction;

	/**
	 * Public constructor.
	 *
	 * @param numerator
	 * 		the numerator value of the RatioStruct
	 * @param denominator
	 * 		the denominator value of the RatioStruct
	 */
	public RatioStruct(final BigInteger numerator, final BigInteger denominator) {
		this(new BigFraction(numerator, denominator));
	}

	/**
	 * Public constructor.
	 *
	 * @param bigFraction
	 * 		the value of the RatioStruct
	 */
	public RatioStruct(final BigFraction bigFraction) {
		super(RatioType.INSTANCE, null, null);
		this.bigFraction = bigFraction;
	}

	/**
	 * Getter for ratio {@link #bigFraction} property.
	 *
	 * @return ratio {@link #bigFraction} property
	 */
	public BigFraction getBigFraction() {
		return bigFraction;
	}

	@Override
	public boolean lispEql(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean lispEqual(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean lispEqualp(final LispStruct lispStruct) {
		return (lispStruct instanceof NumberStruct) && isEqualTo((NumberStruct) lispStruct);
	}

	@Override
	public RealStruct abs() {
		final BigFraction abs = bigFraction.abs();
		if (abs.equals(bigFraction)) {
			return this;
		}
		return new RatioStruct(abs);
	}

	@Override
	public boolean zerop() {
		return bigFraction.compareTo(BigFraction.ZERO) == 0;
	}

	@Override
	public boolean plusp() {
		return bigFraction.compareTo(BigFraction.ZERO) > 0;
	}

	@Override
	public boolean minusp() {
		return bigFraction.compareTo(BigFraction.ZERO) < 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		return RatioAddStrategy.INSTANCE.add(this, number);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		return RatioSubtractStrategy.INSTANCE.subtract(this, number);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		return RatioMultiplyStrategy.INSTANCE.multiply(this, number);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		return RatioDivideStrategy.INSTANCE.divide(this, number);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		return RatioEqualToStrategy.INSTANCE.equalTo(this, number);
	}

	@Override
	public boolean isLessThan(final RealStruct real) {
		return RatioLessThanStrategy.INSTANCE.lessThan(this, real);
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		return RatioGreaterThanStrategy.INSTANCE.greaterThan(this, real);
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		return RatioLessThanOrEqualToStrategy.INSTANCE.lessThanOrEqualTo(this, real);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		return RatioGreaterThanOrEqualToStrategy.INSTANCE.greaterThanOrEqualTo(this, real);
	}

	@Override
	public NumberStruct signum() {
		if (zerop()) {
			return this;
		} else if (plusp()) {
			return IntegerStruct.ONE;
		} else {
			return IntegerStruct.MINUS_ONE;
		}
	}

	@Override
	public RealStruct imagPart() {
		return ZERO;
	}

	@Override
	public NumberStruct negation() {
		return new RatioStruct(bigFraction.negate());
	}

	@Override
	public NumberStruct reciprocal() {
		return makeRational(bigFraction.getNumerator(), bigFraction.getDenominator());
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			if (power instanceof IntegerStruct) {
				return IntegerStruct.ONE;
			}
			return FloatStruct.ONE;
		}

		if (zerop() || isEqualTo(ONE)) {
			return this;
		}

		return RealExptStrategy.INSTANCE.expt(this, power);
	}

	@Override
	public double doubleValue() {
		return bigFraction.doubleValue();
	}

	@Override
	public BigDecimal bigDecimalValue() {
		try {
			return bigFraction.bigDecimalValue();
		} catch (final ArithmeticException ignore) {
			// This means that we have to round the fraction.
			return bigFraction.bigDecimalValue(MathContext.DECIMAL128.getPrecision(), RoundingMode.HALF_EVEN.ordinal());
		}
	}

	@Override
	public RealStruct zeroValue() {
		return ZERO;
	}

	@Override
	public RationalStruct rational() {
		return this;
	}

	@Override
	public QuotientRemainderResult floor(final RealStruct divisor) {
		return RatioQuotientRemainderStrategy.INSTANCE.floor(this, divisor);
	}

	@Override
	public QuotientRemainderResult ffloor(final RealStruct divisor) {
		return RatioQuotientRemainderStrategy.INSTANCE.ffloor(this, divisor);
	}

	@Override
	public QuotientRemainderResult ceiling(final RealStruct divisor) {
		return RatioQuotientRemainderStrategy.INSTANCE.ceiling(this, divisor);
	}

	@Override
	public QuotientRemainderResult fceiling(final RealStruct divisor) {
		return RatioQuotientRemainderStrategy.INSTANCE.fceiling(this, divisor);
	}

	@Override
	public QuotientRemainderResult round(final RealStruct divisor) {
		return RatioQuotientRemainderStrategy.INSTANCE.round(this, divisor);
	}

	@Override
	public QuotientRemainderResult fround(final RealStruct divisor) {
		return RatioQuotientRemainderStrategy.INSTANCE.fround(this, divisor);
	}

	@Override
	public IntegerStruct numerator() {
		return new IntegerStruct(bigFraction.getNumerator());
	}

	@Override
	public IntegerStruct denominator() {
		return new IntegerStruct(bigFraction.getDenominator());
	}

	// Strategy Implementations

	/**
	 * {@link RealAddStrategy} for computing addition results for {@link RatioStruct}s.
	 */
	private static class RatioAddStrategy extends RealAddStrategy<RatioStruct> {

		/**
		 * Singleton instance of the {@link RatioAddStrategy} type.
		 */
		private static final RatioAddStrategy INSTANCE = new RatioAddStrategy();

		@Override
		public RealStruct add(final RatioStruct number1, final IntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigFraction add = bigFraction1.add(bigInteger2);
			return makeRational(add);
		}

		@Override
		public RealStruct add(final RatioStruct number1, final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction add = bigFraction1.add(bigFraction2);
			return makeRational(add);
		}
	}

	/**
	 * {@link RealSubtractStrategy} for computing subtraction function results for {@link RatioStruct}s.
	 */
	private static class RatioSubtractStrategy extends RealSubtractStrategy<RatioStruct> {

		/**
		 * Singleton instance of the {@link RatioSubtractStrategy} type.
		 */
		private static final RatioSubtractStrategy INSTANCE = new RatioSubtractStrategy();

		@Override
		public RealStruct subtract(final RatioStruct number1, final IntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigFraction subtract = bigFraction1.subtract(bigInteger2);
			return makeRational(subtract);
		}

		@Override
		public RealStruct subtract(final RatioStruct number1, final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction subtract = bigFraction1.subtract(bigFraction2);
			return makeRational(subtract);
		}
	}

	/**
	 * {@link RealMultiplyStrategy} for computing multiplication function results for {@link RatioStruct}s.
	 */
	private static class RatioMultiplyStrategy extends RealMultiplyStrategy<RatioStruct> {

		/**
		 * Singleton instance of the {@link RatioMultiplyStrategy} type.
		 */
		private static final RatioMultiplyStrategy INSTANCE = new RatioMultiplyStrategy();

		@Override
		public RealStruct multiply(final RatioStruct number1, final IntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigFraction multiply = bigFraction1.multiply(bigInteger2);
			return makeRational(multiply);
		}

		@Override
		public RealStruct multiply(final RatioStruct number1, final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction multiply = bigFraction1.multiply(bigFraction2);
			return makeRational(multiply);
		}
	}

	/**
	 * {@link RealDivideStrategy} for computing division function results for {@link RatioStruct}s.
	 */
	private static class RatioDivideStrategy extends RealDivideStrategy<RatioStruct> {

		/**
		 * Singleton instance of the {@link RatioDivideStrategy} type.
		 */
		private static final RatioDivideStrategy INSTANCE = new RatioDivideStrategy();

		@Override
		public RealStruct divide(final RatioStruct number1, final IntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigFraction divide = bigFraction1.divide(bigInteger2);
			return makeRational(divide);
		}

		@Override
		public RealStruct divide(final RatioStruct number1, final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction divide = bigFraction1.divide(bigFraction2);
			return makeRational(divide);
		}
	}

	/**
	 * Determines numeric comparison result between the provided RatioStructs.
	 *
	 * @param number1
	 * 		the first RatioStruct in the comparison operation
	 * @param number2
	 * 		the second RatioStruct in the comparison operation
	 *
	 * @return numeric comparison result between the provided RatioStructs
	 */
	private static int getComparisonResult(final RatioStruct number1, final RatioStruct number2) {
		final BigFraction bigFraction1 = number1.bigFraction;
		final BigFraction bigFraction2 = number2.bigFraction;
		return bigFraction1.compareTo(bigFraction2);
	}

	/**
	 * Determines numeric comparison result between the provided RatioStruct and {@link IntegerStruct}.
	 *
	 * @param number1
	 * 		the RatioStruct in the comparison operation
	 * @param number2
	 * 		the {@link IntegerStruct} in the comparison operation
	 *
	 * @return numeric comparison result between the provided RatioStruct and {@link IntegerStruct}
	 */
	private static int getComparisonResult(final RatioStruct number1, final IntegerStruct number2) {

		final BigFraction bigFraction1 = number1.bigFraction;
		final BigFraction bigFraction1Reduced = bigFraction1.reduce();
		final BigInteger numerator = bigFraction1Reduced.getNumerator();
		final BigInteger denominator = bigFraction1Reduced.getDenominator();

		final BigInteger bigInteger2 = number2.getBigInteger();
		final BigInteger multiply = bigInteger2.multiply(denominator);
		return numerator.compareTo(multiply);
	}

	/**
	 * {@link RealEqualToStrategy} for computing numeric '=' equality results for {@link RatioStruct}s.
	 */
	private static class RatioEqualToStrategy extends RealEqualToStrategy<RatioStruct> {

		/**
		 * Singleton instance of the {@link RatioEqualToStrategy} type.
		 */
		private static final RatioEqualToStrategy INSTANCE = new RatioEqualToStrategy();

		@Override
		public boolean equalTo(final RatioStruct number1, final IntegerStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		@Override
		public boolean equalTo(final RatioStruct number1, final RatioStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}
	}

	/**
	 * {@link LessThanStrategy} for computing numeric '<' equality results for {@link RatioStruct}s.
	 */
	private static class RatioLessThanStrategy extends LessThanStrategy<RatioStruct> {

		/**
		 * Singleton instance of the {@link RatioLessThanStrategy} type.
		 */
		private static final RatioLessThanStrategy INSTANCE = new RatioLessThanStrategy();

		@Override
		public boolean lessThan(final RatioStruct real1, final IntegerStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		@Override
		public boolean lessThan(final RatioStruct real1, final RatioStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}
	}

	/**
	 * {@link GreaterThanStrategy} for computing numeric '>' equality results for {@link RatioStruct}s.
	 */
	private static class RatioGreaterThanStrategy extends GreaterThanStrategy<RatioStruct> {

		/**
		 * Singleton instance of the {@link RatioGreaterThanStrategy} type.
		 */
		private static final RatioGreaterThanStrategy INSTANCE = new RatioGreaterThanStrategy();

		@Override
		public boolean greaterThan(final RatioStruct real1, final IntegerStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		@Override
		public boolean greaterThan(final RatioStruct real1, final RatioStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}
	}

	/**
	 * {@link LessThanOrEqualToStrategy} for computing numeric '<=' equality results for {@link RatioStruct}s.
	 */
	private static class RatioLessThanOrEqualToStrategy extends LessThanOrEqualToStrategy<RatioStruct> {

		/**
		 * Singleton instance of the {@link RatioLessThanOrEqualToStrategy} type.
		 */
		private static final RatioLessThanOrEqualToStrategy INSTANCE = new RatioLessThanOrEqualToStrategy();

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real1, final IntegerStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real1, final RatioStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}
	}

	/**
	 * {@link GreaterThanOrEqualToStrategy} for computing numeric '>=' equality results for {@link RatioStruct}s.
	 */
	private static class RatioGreaterThanOrEqualToStrategy extends GreaterThanOrEqualToStrategy<RatioStruct> {

		/**
		 * Singleton instance of the {@link RatioGreaterThanOrEqualToStrategy} type.
		 */
		private static final RatioGreaterThanOrEqualToStrategy INSTANCE = new RatioGreaterThanOrEqualToStrategy();

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real1, final IntegerStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real1, final RatioStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}
	}

	/**
	 * {@link RationalQuotientRemainderStrategy} for computing quotient and remainder results for {@link RatioStruct}s.
	 */
	private static class RatioQuotientRemainderStrategy extends RationalQuotientRemainderStrategy<RatioStruct> {

		/**
		 * Singleton instance of the {@link RatioQuotientRemainderStrategy} type.
		 */
		private static final RatioQuotientRemainderStrategy INSTANCE = new RatioQuotientRemainderStrategy();

		@Override
		public QuotientRemainderResult quotientRemainder(final RatioStruct real, final IntegerStruct divisor,
		                                                 final RoundingMode roundingMode,
		                                                 final boolean isFloatResult) {
			return ratioQuotientRemainder(real, divisor, roundingMode, isFloatResult);
		}
	}

	// HashCode / Equals

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bigFraction)
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
		final RatioStruct rhs = (RatioStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigFraction, rhs.bigFraction)
		                          .isEquals();
	}
}

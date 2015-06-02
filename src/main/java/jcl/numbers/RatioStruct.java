/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.LispStruct;
import jcl.types.RatioType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public class RatioStruct extends RationalStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2468768422160538347L;

	public static final RatioStruct ZERO = new RatioStruct(BigFraction.ZERO);

	public static final RatioStruct ONE = new RatioStruct(BigFraction.ONE);

	public static final RatioStruct MINUS_ONE = new RatioStruct(BigFraction.MINUS_ONE);

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
		final BigFraction negate = bigFraction.negate();
		return new RatioStruct(negate);
	}

	@Override
	public NumberStruct reciprocal() {
		final BigInteger numerator = bigFraction.getNumerator();
		final BigInteger denominator = bigFraction.getDenominator();
		return makeRational(denominator, numerator);
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
		return bigFraction.bigDecimalValue();
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
	public IntegerStruct numerator() {
		return new IntegerStruct(bigFraction.getNumerator());
	}

	@Override
	public IntegerStruct denominator() {
		return new IntegerStruct(bigFraction.getDenominator());
	}

	// Strategy Implementations

	private static class RatioAddStrategy extends RealAddStrategy<RatioStruct> {

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

	private static class RatioSubtractStrategy extends RealSubtractStrategy<RatioStruct> {

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

	private static class RatioMultiplyStrategy extends RealMultiplyStrategy<RatioStruct> {

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

	private static class RatioDivideStrategy extends RealDivideStrategy<RatioStruct> {

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

	private static class RatioEqualToStrategy extends EqualToStrategy<RatioStruct> {

		private static final RatioEqualToStrategy INSTANCE = new RatioEqualToStrategy();

		@Override
		public boolean equalTo(final RatioStruct number1, final IntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();

			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigFraction bigFraction2 = new BigFraction(bigInteger2);

			return bigFraction1.equals(bigFraction2);
		}

		@Override
		public boolean equalTo(final RatioStruct number1, final FloatStruct number2) {
			final RationalStruct rational2 = number2.rational();
			return number1.isEqualTo(rational2);
		}

		@Override
		public boolean equalTo(final RatioStruct number1, final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			return bigFraction1.equals(bigFraction2);
		}

		@Override
		public boolean equalTo(final RatioStruct number1, final ComplexStruct number2) {
			return false;
		}
	}

	private static class RatioLessThanStrategy extends LessThanStrategy<RatioStruct> {

		private static final RatioLessThanStrategy INSTANCE = new RatioLessThanStrategy();

		@Override
		public boolean lessThan(final RatioStruct real1, final IntegerStruct real2) {
			final BigFraction bigFraction1 = real1.getBigFraction();

			final BigInteger bigInteger2 = real2.getBigInteger();
			final BigFraction bigFraction2 = new BigFraction(bigInteger2);

			return bigFraction1.compareTo(bigFraction2) < 0;
		}

		@Override
		public boolean lessThan(final RatioStruct real1, final FloatStruct real2) {
			final RationalStruct rational2 = real2.rational();
			return real1.isLessThan(rational2);
		}

		@Override
		public boolean lessThan(final RatioStruct real1, final RatioStruct real2) {
			final BigFraction bigFraction1 = real1.getBigFraction();
			final BigFraction bigFraction2 = real2.getBigFraction();
			return bigFraction1.compareTo(bigFraction2) < 0;
		}
	}

	private static class RatioGreaterThanStrategy extends GreaterThanStrategy<RatioStruct> {

		private static final RatioGreaterThanStrategy INSTANCE = new RatioGreaterThanStrategy();

		@Override
		public boolean greaterThan(final RatioStruct real1, final IntegerStruct real2) {
			final BigFraction bigFraction1 = real1.getBigFraction();

			final BigInteger bigInteger2 = real2.getBigInteger();
			final BigFraction bigFraction2 = new BigFraction(bigInteger2);

			return bigFraction1.compareTo(bigFraction2) > 0;
		}

		@Override
		public boolean greaterThan(final RatioStruct real1, final FloatStruct real2) {
			final RationalStruct rational2 = real2.rational();
			return real1.isGreaterThan(rational2);
		}

		@Override
		public boolean greaterThan(final RatioStruct real1, final RatioStruct real2) {
			final BigFraction bigFraction1 = real1.getBigFraction();
			final BigFraction bigFraction2 = real2.getBigFraction();
			return bigFraction1.compareTo(bigFraction2) > 0;
		}
	}

	private static class RatioLessThanOrEqualToStrategy extends LessThanOrEqualToStrategy<RatioStruct> {

		private static final RatioLessThanOrEqualToStrategy INSTANCE = new RatioLessThanOrEqualToStrategy();

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real1, final IntegerStruct real2) {
			final BigFraction bigFraction1 = real1.getBigFraction();

			final BigInteger bigInteger2 = real2.getBigInteger();
			final BigFraction bigFraction2 = new BigFraction(bigInteger2);

			return bigFraction1.compareTo(bigFraction2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real1, final FloatStruct real2) {
			final RationalStruct rational2 = real2.rational();
			return real1.isLessThanOrEqualTo(rational2);
		}

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real1, final RatioStruct real2) {
			final BigFraction bigFraction1 = real1.getBigFraction();
			final BigFraction bigFraction2 = real2.getBigFraction();
			return bigFraction1.compareTo(bigFraction2) <= 0;
		}
	}

	private static class RatioGreaterThanOrEqualToStrategy extends GreaterThanOrEqualToStrategy<RatioStruct> {

		private static final RatioGreaterThanOrEqualToStrategy INSTANCE = new RatioGreaterThanOrEqualToStrategy();

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real1, final IntegerStruct real2) {
			final BigFraction bigFraction1 = real1.getBigFraction();

			final BigInteger bigInteger2 = real2.getBigInteger();
			final BigFraction bigFraction2 = new BigFraction(bigInteger2);

			return bigFraction1.compareTo(bigFraction2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real1, final FloatStruct real2) {
			final RationalStruct rational2 = real2.rational();
			return real1.isGreaterThanOrEqualTo(rational2);
		}

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real1, final RatioStruct real2) {
			final BigFraction bigFraction1 = real1.getBigFraction();
			final BigFraction bigFraction2 = real2.getBigFraction();
			return bigFraction1.compareTo(bigFraction2) >= 0;
		}
	}

	// HashCode / Equals / ToString

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

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bigFraction)
		                                                                .toString();
	}
}

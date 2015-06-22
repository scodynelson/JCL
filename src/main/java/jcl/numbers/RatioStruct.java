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
import org.apfloat.Apfloat;

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

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the absolute value of this RatioStruct.
	 */
	@Override
	public RealStruct abs() {
		final BigFraction abs = bigFraction.abs();
		if (abs.compareTo(bigFraction) == 0) {
			return this;
		}
		return new RatioStruct(abs);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this RatioStruct is zero by comparing {@link #bigFraction} to {@link
	 * BigFraction#ZERO}.
	 */
	@Override
	public boolean zerop() {
		return BigFraction.ZERO.compareTo(bigFraction) == 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this RatioStruct is positive by comparing {@link #bigFraction} to {@link
	 * BigFraction#ZERO}.
	 */
	@Override
	public boolean plusp() {
		return BigFraction.ZERO.compareTo(bigFraction) > 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this RatioStruct is negative by comparing {@link #bigFraction} to {@link
	 * BigFraction#ZERO}.
	 */
	@Override
	public boolean minusp() {
		return BigFraction.ZERO.compareTo(bigFraction) < 0;
	}

	@Override
	protected NumberStruct add(final AddVisitor<?> addVisitor) {
		return addVisitor.add(this);
	}

	@Override
	protected AddVisitor<?> addVisitor() {
		return new RatioAddVisitor(this);
	}

	@Override
	protected NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	protected SubtractVisitor<?> subtractVisitor() {
		return new RatioSubtractVisitor(this);
	}

	@Override
	protected NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	protected MultiplyVisitor<?> multiplyVisitor() {
		return new RatioMultiplyVisitor(this);
	}

	@Override
	protected NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	protected DivideVisitor<?> divideVisitor() {
		return new RatioDivideVisitor(this);
	}

	@Override
	protected boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	@Override
	protected EqualToVisitor<?> equalToVisitor() {
		return new RatioEqualToVisitor(this);
	}

	@Override
	protected boolean isLessThan(final LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	@Override
	protected LessThanVisitor<?> lessThanVisitor() {
		return new RatioLessThanVisitor(this);
	}

	@Override
	protected boolean isGreaterThan(final GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	@Override
	protected GreaterThanVisitor<?> greaterThanVisitor() {
		return new RatioGreaterThanVisitor(this);
	}

	@Override
	protected boolean isLessThanOrEqualTo(final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	@Override
	protected LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new RatioLessThanOrEqualToVisitor(this);
	}

	@Override
	protected boolean isGreaterThanOrEqualTo(final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	@Override
	protected GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new RatioGreaterThanOrEqualToVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the whether or not the numerical value of this RatioStruct is zero, positive, or negative,
	 * returning {@code this}, {@link IntegerStruct#ONE}, or {@link IntegerStruct#MINUS_ONE} respectively.
	 */
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

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #ZERO} as the imaginary part of RatioStructs is always '0'.
	 */
	@Override
	public RealStruct imagPart() {
		return ZERO;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the negation with {@link #bigFraction#negation} and the creating a new RatioStruct to wrap it.
	 */
	@Override
	public NumberStruct negation() {
		final BigFraction negate = bigFraction.negate();
		return new RatioStruct(negate);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Creates a new {@link RationalStruct} with {@link #bigFraction#denominator} as the numerator and {@link
	 * #bigFraction#numerator} as the denominator.
	 */
	@Override
	public NumberStruct reciprocal() {
		return makeRational(bigFraction.getDenominator(), bigFraction.getNumerator());
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the exponential function result for this RatioStruct as this {@code base} and the provided {@link
	 * NumberStruct} as the {@code power}. If {@code power} is '0' and power is an {@link IntegerStruct}, {@link
	 * IntegerStruct#ONE} is returned. If {@code power} is '0' and power is not an {@link IntegerStruct}, {@link
	 * FloatStruct#ONE} is returned. If this RatioStruct is either '0' or '1', {@code this} is returned.
	 */
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

		return super.expt(power);
	}

	@Override
	protected NumberStruct expt(final ExptVisitor<?> exptVisitor) {
		return exptVisitor.expt(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes a {@link BigDecimal} value from the {@link #bigFraction} by first attempting {@link
	 * #bigFraction#bigDecimalValue()}. If that fails with an {@link ArithmeticException}, the {@link BigDecimal} is
	 * then attempted to be computed with {@link #bigFraction#bigDecimalValue(int, int)} passing {@link
	 * MathContext#DECIMAL128} as the scale and {@link RoundingMode#HALF_EVEN} as the rounding mode.
	 */
	@Override
	public BigDecimal bigDecimalValue() {
		try {
			return bigFraction.bigDecimalValue();
		} catch (final ArithmeticException ignore) {
			// This means that we have to round the fraction.
			final int scale = MathContext.DECIMAL128.getPrecision();
			final int roundingMode = RoundingMode.HALF_EVEN.ordinal();
			return bigFraction.bigDecimalValue(scale, roundingMode);
		}
	}

	@Override
	public Apfloat apfloatValue() {
		final BigDecimal bigDecimal = bigDecimalValue();
		return new Apfloat(bigDecimal);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #ZERO} as a '0' RatioStruct value.
	 */
	@Override
	public RealStruct zeroValue() {
		return ZERO;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@code this} as any RatioStruct is already in rational form.
	 */
	@Override
	public RationalStruct rational() {
		return this;
	}

	@Override
	protected QuotientRemainderResult floor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.floor(this);
	}

	@Override
	protected QuotientRemainderResult ffloor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ffloor(this);
	}

	@Override
	protected QuotientRemainderResult ceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ceiling(this);
	}

	@Override
	protected QuotientRemainderResult fceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fceiling(this);
	}

	@Override
	protected QuotientRemainderResult round(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.round(this);
	}

	@Override
	protected QuotientRemainderResult fround(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fround(this);
	}

	@Override
	protected QuotientRemainderVisitor<?> quotientRemainderVisitor() {
		return new RatioQuotientRemainderVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #bigFraction#numerator} as a new {@link IntegerStruct}.
	 */
	@Override
	public IntegerStruct numerator() {
		return new IntegerStruct(bigFraction.getNumerator());
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #bigFraction#denominator} as a new {@link IntegerStruct}.
	 */
	@Override
	public IntegerStruct denominator() {
		return new IntegerStruct(bigFraction.getDenominator());
	}

	// Visitor Implementations

	/**
	 * {@link RealAddVisitor} for computing addition results for {@link RatioStruct}s.
	 */
	private static final class RatioAddVisitor extends RealAddVisitor<RatioStruct> {

		/**
		 * Private constructor to make a new instance of an RatioAddVisitor with the provided {@link RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		private RatioAddVisitor(final RatioStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct add(final IntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigFraction add = bigFraction1.add(bigInteger2);
			return makeRational(add);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for {@link RatioStruct}s.
		 */
		@Override
		public RealStruct add(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction add = bigFraction1.add(bigFraction2);
			return makeRational(add);
		}
	}

	/**
	 * {@link RealSubtractVisitor} for computing subtraction function results for {@link RatioStruct}s.
	 */
	private static final class RatioSubtractVisitor extends RealSubtractVisitor<RatioStruct> {

		/**
		 * Private constructor to make a new instance of an RatioSubtractVisitor with the provided {@link RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		private RatioSubtractVisitor(final RatioStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct subtract(final IntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigFraction subtract = bigFraction1.subtract(bigInteger2);
			return makeRational(subtract);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for {@link RatioStruct}s.
		 */
		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction subtract = bigFraction1.subtract(bigFraction2);
			return makeRational(subtract);
		}
	}

	/**
	 * {@link RealMultiplyVisitor} for computing multiplication function results for {@link RatioStruct}s.
	 */
	private static final class RatioMultiplyVisitor extends RealMultiplyVisitor<RatioStruct> {

		/**
		 * Private constructor to make a new instance of an RatioMultiplyVisitor with the provided {@link RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		private RatioMultiplyVisitor(final RatioStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct multiply(final IntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigFraction multiply = bigFraction1.multiply(bigInteger2);
			return makeRational(multiply);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for {@link RatioStruct}s.
		 */
		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction multiply = bigFraction1.multiply(bigFraction2);
			return makeRational(multiply);
		}
	}

	/**
	 * {@link RealDivideVisitor} for computing division function results for {@link RatioStruct}s.
	 */
	private static final class RatioDivideVisitor extends RealDivideVisitor<RatioStruct> {

		/**
		 * Private constructor to make a new instance of an RatioDivideVisitor with the provided {@link RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		private RatioDivideVisitor(final RatioStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct divide(final IntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigFraction divide = bigFraction1.divide(bigInteger2);
			return makeRational(divide);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for {@link RatioStruct}s.
		 */
		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction divide = bigFraction1.divide(bigFraction2);
			return makeRational(divide);
		}
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
	 * {@link RealEqualToVisitor} for computing numeric '=' equality results for {@link RatioStruct}s.
	 */
	private static final class RatioEqualToVisitor extends RealEqualToVisitor<RatioStruct> {

		/**
		 * Private constructor to make a new instance of an RatioEqualToVisitor with the provided {@link RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		private RatioEqualToVisitor(final RatioStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '=' equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public boolean equalTo(final IntegerStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '=' equality result for {@link RatioStruct}s.
		 */
		@Override
		public boolean equalTo(final RatioStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}
	}

	/**
	 * {@link LessThanVisitor} for computing numeric '<' equality results for {@link RatioStruct}s.
	 */
	private static final class RatioLessThanVisitor extends LessThanVisitor<RatioStruct> {

		/**
		 * Private constructor to make a new instance of an RatioLessThanVisitor with the provided {@link RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric '<' equality operation
		 */
		private RatioLessThanVisitor(final RatioStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '<' equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public boolean lessThan(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '<' equality result for {@link RatioStruct}s.
		 */
		@Override
		public boolean lessThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}
	}

	/**
	 * {@link GreaterThanVisitor} for computing numeric '>' equality results for {@link RatioStruct}s.
	 */
	private static final class RatioGreaterThanVisitor extends GreaterThanVisitor<RatioStruct> {

		/**
		 * Private constructor to make a new instance of an RatioGreaterThanVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric '>' equality operation
		 */
		private RatioGreaterThanVisitor(final RatioStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '>' equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public boolean greaterThan(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '>' equality result for {@link RatioStruct}s.
		 */
		@Override
		public boolean greaterThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}
	}

	/**
	 * {@link LessThanOrEqualToVisitor} for computing numeric '<=' equality results for {@link RatioStruct}s.
	 */
	private static final class RatioLessThanOrEqualToVisitor extends LessThanOrEqualToVisitor<RatioStruct> {

		/**
		 * Private constructor to make a new instance of an RatioLessThanOrEqualToVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric '<=' equality operation
		 */
		private RatioLessThanOrEqualToVisitor(final RatioStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '<=' equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public boolean lessThanOrEqualTo(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '<=' equality result for {@link RatioStruct}s.
		 */
		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}
	}

	/**
	 * {@link GreaterThanOrEqualToVisitor} for computing numeric '>=' equality results for {@link RatioStruct}s.
	 */
	private static final class RatioGreaterThanOrEqualToVisitor extends GreaterThanOrEqualToVisitor<RatioStruct> {

		/**
		 * Private constructor to make a new instance of an RatioGreaterThanOrEqualToVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric '>=' equality operation
		 */
		private RatioGreaterThanOrEqualToVisitor(final RatioStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '>=' equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public boolean greaterThanOrEqualTo(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '>=' equality result for {@link RatioStruct}s.
		 */
		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}
	}

	/**
	 * {@link RationalQuotientRemainderVisitor} for computing quotient and remainder results for {@link RatioStruct}s.
	 */
	private static final class RatioQuotientRemainderVisitor extends RationalQuotientRemainderVisitor<RatioStruct> {

		/**
		 * Private constructor to make a new instance of an RatioQuotientRemainderVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param real
		 * 		the real argument in the computational quotient and remainder operation
		 */
		private RatioQuotientRemainderVisitor(final RatioStruct real) {
			super(real);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the quotient and remainder results for a {@link RatioStruct} as the {@code real} and an {@link
		 * IntegerStruct} as the {@code divisor}.
		 */
		@Override
		public QuotientRemainderResult quotientRemainder(final IntegerStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			return ratioQuotientRemainder(divisor, roundingMode, isQuotientFloat);
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

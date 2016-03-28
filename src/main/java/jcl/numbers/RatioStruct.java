/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

import jcl.classes.BuiltInClassStruct;
import jcl.types.RatioType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.Aprational;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public final class RatioStruct extends BuiltInClassStruct implements RationalStruct {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(RatioStruct.class);

	/**
	 * The internal {@link BigFraction} containing the ratio contents.
	 */
	final BigFraction bigFraction;

	/**
	 * Private constructor.
	 *
	 * @param bigFraction
	 * 		the value of the RatioStruct
	 */
	private RatioStruct(final BigFraction bigFraction) {
		super(RatioType.INSTANCE, null, null);
		this.bigFraction = bigFraction;
	}

	public static RatioStruct valueOf(final BigFraction bigFraction) {
		return new RatioStruct(bigFraction);
	}

	public static RatioStruct valueOf(final BigInteger numerator, final BigInteger denominator) {
		final BigFraction bigFraction = new BigFraction(numerator, denominator);
		return new RatioStruct(bigFraction);
	}

	/**
	 * Getter for ratio {@link #bigFraction} property.
	 *
	 * @return ratio {@link #bigFraction} property
	 */
	@Deprecated
	public BigFraction getBigFraction() {
		return bigFraction;
	}

	public float floatValue() {
		// TODO: should we use this? possible precision loss?
		return bigFraction.floatValue();
	}

	public double doubleValue() {
		// TODO: should we use this? possible precision loss?
		return bigFraction.doubleValue();
	}

	public BigDecimal bigDecimalValue2() {
		// TODO: make this the main one later
		try {
			return bigFraction.bigDecimalValue();
		} catch (final ArithmeticException ignore) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Loss of precision when converting BigFraction to BigDecimal.");
			}
			// This means that we have to round the fraction.
			final int scale = MathContext.DECIMAL128.getPrecision();
			final int roundingMode = RoundingMode.HALF_EVEN.ordinal();
			return bigFraction.bigDecimalValue(scale, roundingMode);
		}
	}

	@Override
	public RealStruct abs() {
		final BigInteger numerator = bigFraction.getNumerator();
		if (numerator.signum() >= 0) {
			return this;
		}
		return negation();
	}

	@Override
	public boolean zerop() {
		return BigFraction.ZERO.compareTo(bigFraction) == 0;
	}

	@Override
	public boolean plusp() {
		return BigFraction.ZERO.compareTo(bigFraction) > 0;
	}

	@Override
	public boolean minusp() {
		return BigFraction.ZERO.compareTo(bigFraction) < 0;
	}

	@Override
	public NumberStruct add(final AddVisitor<?> addVisitor) {
		return addVisitor.add(this);
	}

	@Override
	public AddVisitor<?> addVisitor() {
		return new RatioAddVisitor(this);
	}

	@Override
	public NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	public SubtractVisitor<?> subtractVisitor() {
		return new RatioSubtractVisitor(this);
	}

	@Override
	public NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	public MultiplyVisitor<?> multiplyVisitor() {
		return new RatioMultiplyVisitor(this);
	}

	@Override
	public NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	public DivideVisitor<?> divideVisitor() {
		return new RatioDivideVisitor(this);
	}

	@Override
	public boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	@Override
	public EqualToVisitor<?> equalToVisitor() {
		return new RatioEqualToVisitor(this);
	}

	@Override
	public boolean isLessThan(final RealStruct.LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	@Override
	public RealStruct.LessThanVisitor<?> lessThanVisitor() {
		return new RatioLessThanVisitor(this);
	}

	@Override
	public boolean isGreaterThan(final RealStruct.GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	@Override
	public RealStruct.GreaterThanVisitor<?> greaterThanVisitor() {
		return new RatioGreaterThanVisitor(this);
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	@Override
	public RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new RatioLessThanOrEqualToVisitor(this);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	@Override
	public RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new RatioGreaterThanOrEqualToVisitor(this);
	}

	@Override
	public RatioStruct negation() {
		final BigFraction negate = bigFraction.negate();
		return new RatioStruct(negate);
	}

	@Override
	public NumberStruct reciprocal() {
		return RationalStruct.makeRational(bigFraction.getDenominator(), bigFraction.getNumerator());
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			if (power instanceof IntegerStruct) {
				return IntegerStruct.ONE;
			}
			return FloatStruct.ONE;
		}

		if (zerop() || isEqualTo(IntegerStruct.ONE)) {
			return this;
		}

		final ExptVisitor<?> exptVisitor = exptVisitor();
		return power.expt(exptVisitor);
	}

	@Override
	public NumberStruct expt(final ExptVisitor<?> exptVisitor) {
		return exptVisitor.expt(this);
	}

	@Override
	@Deprecated
	public BigDecimal bigDecimalValue() {
		try {
			return bigFraction.bigDecimalValue();
		} catch (final ArithmeticException ignore) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Loss of precision when converting BigFraction to BigDecimal.");
			}
			// This means that we have to round the fraction.
			final int scale = MathContext.DECIMAL128.getPrecision();
			final int roundingMode = RoundingMode.HALF_EVEN.ordinal();
			return bigFraction.bigDecimalValue(scale, roundingMode);
		}
	}

	@Override
	@Deprecated
	public Apfloat apfloatValue() {
		final Apint apintNumerator = new Apint(bigFraction.getNumerator());
		final Apint apintDenominator = new Apint(bigFraction.getDenominator());
		return new Aprational(apintNumerator, apintDenominator);
	}

	@Override
	public FloatStruct coerceRealToFloat() {
		return FloatStruct.valueOf(bigDecimalValue());
	}

	@Override
	public QuotientRemainderResult floor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.floor(this);
	}

	@Override
	public QuotientRemainderResult ffloor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ffloor(this);
	}

	@Override
	public QuotientRemainderResult ceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ceiling(this);
	}

	@Override
	public QuotientRemainderResult fceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fceiling(this);
	}

	@Override
	public QuotientRemainderResult round(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.round(this);
	}

	@Override
	public QuotientRemainderResult fround(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fround(this);
	}

	@Override
	public QuotientRemainderVisitor<?> quotientRemainderVisitor() {
		return new RatioQuotientRemainderVisitor(this);
	}

	@Override
	public IntegerStruct numerator() {
		return IntegerStruct.valueOf(bigFraction.getNumerator());
	}

	@Override
	public IntegerStruct denominator() {
		return IntegerStruct.valueOf(bigFraction.getDenominator());
	}

	// Comparison Visitor Helpers

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

		final BigInteger bigInteger2 = number2.bigIntegerValue();
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

	// HashCode / Equals

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(bigFraction)
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
		return new EqualsBuilder().append(bigFraction, rhs.bigFraction)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return bigFraction.toString();
	}

	// Visitor Implementations

	/**
	 * {@link RealStruct.RealAddVisitor} for computing addition results for {@link RatioStruct}s.
	 */
	private static final class RatioAddVisitor extends RealStruct.RealAddVisitor<RatioStruct> {

		/**
		 * Package private constructor to make a new instance of an RatioAddVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		RatioAddVisitor(final RatioStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct add(final IntIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigIntegerValue();
			final BigFraction add = bigFraction1.add(bigInteger2);
			return RationalStruct.makeRational(add);
		}

		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigIntegerValue();
			final BigFraction add = bigFraction1.add(bigInteger2);
			return RationalStruct.makeRational(add);
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigInteger;
			final BigFraction add = bigFraction1.add(bigInteger2);
			return RationalStruct.makeRational(add);
		}

		@Override
		public RealStruct add(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction add = bigFraction1.add(bigFraction2);
			return RationalStruct.makeRational(add);
		}
	}

	/**
	 * {@link RealStruct.RealSubtractVisitor} for computing subtraction function results for {@link RatioStruct}s.
	 */
	private static final class RatioSubtractVisitor extends RealStruct.RealSubtractVisitor<RatioStruct> {

		/**
		 * Package private constructor to make a new instance of an RatioSubtractVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		RatioSubtractVisitor(final RatioStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct subtract(final IntIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigIntegerValue();
			final BigFraction subtract = bigFraction1.subtract(bigInteger2);
			return RationalStruct.makeRational(subtract);
		}

		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigIntegerValue();
			final BigFraction subtract = bigFraction1.subtract(bigInteger2);
			return RationalStruct.makeRational(subtract);
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigInteger;
			final BigFraction subtract = bigFraction1.subtract(bigInteger2);
			return RationalStruct.makeRational(subtract);
		}

		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction subtract = bigFraction1.subtract(bigFraction2);
			return RationalStruct.makeRational(subtract);
		}
	}

	/**
	 * {@link RealStruct.RealMultiplyVisitor} for computing multiplication function results for {@link RatioStruct}s.
	 */
	private static final class RatioMultiplyVisitor extends RealStruct.RealMultiplyVisitor<RatioStruct> {

		/**
		 * Package private constructor to make a new instance of an RatioMultiplyVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		RatioMultiplyVisitor(final RatioStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct multiply(final IntIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigIntegerValue();
			final BigFraction multiply = bigFraction1.multiply(bigInteger2);
			return RationalStruct.makeRational(multiply);
		}

		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigIntegerValue();
			final BigFraction multiply = bigFraction1.multiply(bigInteger2);
			return RationalStruct.makeRational(multiply);
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigInteger;
			final BigFraction multiply = bigFraction1.multiply(bigInteger2);
			return RationalStruct.makeRational(multiply);
		}

		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction multiply = bigFraction1.multiply(bigFraction2);
			return RationalStruct.makeRational(multiply);
		}
	}

	/**
	 * {@link RealStruct.RealDivideVisitor} for computing division function results for {@link RatioStruct}s.
	 */
	private static final class RatioDivideVisitor extends RealStruct.RealDivideVisitor<RatioStruct> {

		/**
		 * Package private constructor to make a new instance of an RatioDivideVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		RatioDivideVisitor(final RatioStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct divide(final IntIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigIntegerValue();
			final BigFraction divide = bigFraction1.divide(bigInteger2);
			return RationalStruct.makeRational(divide);
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigIntegerValue();
			final BigFraction divide = bigFraction1.divide(bigInteger2);
			return RationalStruct.makeRational(divide);
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigInteger bigInteger2 = number2.bigInteger;
			final BigFraction divide = bigFraction1.divide(bigInteger2);
			return RationalStruct.makeRational(divide);
		}

		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.getBigFraction();
			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction divide = bigFraction1.divide(bigFraction2);
			return RationalStruct.makeRational(divide);
		}
	}

	/**
	 * {@link RealStruct.RealEqualToVisitor} for computing numeric '=' equality results for {@link RatioStruct}s.
	 */
	private static final class RatioEqualToVisitor extends RealStruct.RealEqualToVisitor<RatioStruct> {

		/**
		 * Package private constructor to make a new instance of an RatioEqualToVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		RatioEqualToVisitor(final RatioStruct number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final IntIntegerStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		@Override
		public boolean equalTo(final LongIntegerStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		@Override
		public boolean equalTo(final BigIntegerStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		@Override
		public boolean equalTo(final RatioStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}
	}

	/**
	 * {@link RealStruct.LessThanVisitor} for computing numeric {@literal '<'} equality results for {@link
	 * RatioStruct}s.
	 */
	private static final class RatioLessThanVisitor extends RealStruct.LessThanVisitor<RatioStruct> {

		/**
		 * Package private constructor to make a new instance of an RatioLessThanVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		RatioLessThanVisitor(final RatioStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThan(final IntIntegerStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		@Override
		public boolean lessThan(final LongIntegerStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		@Override
		public boolean lessThan(final BigIntegerStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		@Override
		public boolean lessThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}
	}

	/**
	 * {@link RealStruct.GreaterThanVisitor} for computing numeric {@literal '>'} equality results for {@link
	 * RatioStruct}s.
	 */
	private static final class RatioGreaterThanVisitor extends RealStruct.GreaterThanVisitor<RatioStruct> {

		/**
		 * Package private constructor to make a new instance of an RatioGreaterThanVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		RatioGreaterThanVisitor(final RatioStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThan(final IntIntegerStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		@Override
		public boolean greaterThan(final LongIntegerStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		@Override
		public boolean greaterThan(final BigIntegerStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		@Override
		public boolean greaterThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}
	}

	/**
	 * {@link RealStruct.LessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality results for {@link
	 * RatioStruct}s.
	 */
	private static final class RatioLessThanOrEqualToVisitor extends RealStruct.LessThanOrEqualToVisitor<RatioStruct> {

		/**
		 * Package private constructor to make a new instance of an RatioLessThanOrEqualToVisitor with the provided
		 * {@link RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		RatioLessThanOrEqualToVisitor(final RatioStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThanOrEqualTo(final IntIntegerStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final LongIntegerStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final BigIntegerStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}
	}

	/**
	 * {@link RealStruct.GreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality results for {@link
	 * RatioStruct}s.
	 */
	private static final class RatioGreaterThanOrEqualToVisitor extends RealStruct.GreaterThanOrEqualToVisitor<RatioStruct> {

		/**
		 * Package private constructor to make a new instance of an RatioGreaterThanOrEqualToVisitor with the provided
		 * {@link RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		RatioGreaterThanOrEqualToVisitor(final RatioStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThanOrEqualTo(final IntIntegerStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final LongIntegerStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigIntegerStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}
	}

	/**
	 * {@link RationalStruct.RationalQuotientRemainderVisitor} for computing quotient and remainder results for {@link
	 * RatioStruct}s.
	 */
	private static final class RatioQuotientRemainderVisitor extends RationalStruct.RationalQuotientRemainderVisitor<RatioStruct> {

		/**
		 * Package private constructor to make a new instance of an RatioQuotientRemainderVisitor with the provided
		 * {@link RatioStruct}.
		 *
		 * @param real
		 * 		the real argument in the computational quotient and remainder operation
		 */
		RatioQuotientRemainderVisitor(final RatioStruct real) {
			super(real);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final IntegerStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			return ratioQuotientRemainder(divisor, roundingMode, isQuotientFloat);
		}
	}
}

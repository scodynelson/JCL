/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

import jcl.classes.BuiltInClassStruct;
import jcl.conditions.exceptions.DivisionByZeroException;
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

	/*
		RationalStruct
	 */

	@Override
	public IntegerStruct numerator() {
		return IntegerStruct.valueOf(bigFraction.getNumerator());
	}

	@Override
	public IntegerStruct denominator() {
		return IntegerStruct.valueOf(bigFraction.getDenominator());
	}

	/*
		RealStruct
	 */

	@Override
	public FloatStruct coerceRealToFloat() {
		// TODO: precision??
		return FloatStruct.valueOf(bigDecimalValue());
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
	public boolean plusp() {
		return BigFraction.ZERO.compareTo(bigFraction) > 0;
	}

	@Override
	public boolean minusp() {
		return BigFraction.ZERO.compareTo(bigFraction) < 0;
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

	/*
		NumberStruct
	 */

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
	public NumberStruct expt(final ExptVisitor<?> exptVisitor) {
		return exptVisitor.expt(this);
	}

	@Override
	public ExptVisitor<?> exptVisitor() {
		return new RatioExptVisitor(this);
	}

	@Override
	public boolean zerop() {
		return BigFraction.ZERO.compareTo(bigFraction) == 0;
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
	public RatioStruct negation() {
		return valueOf(bigFraction.negate());
	}

	@Override
	public NumberStruct reciprocal() {
		final BigInteger numerator = bigFraction.getNumerator();
		if (BigInteger.ZERO.equals(numerator)) {
			throw new DivisionByZeroException("Division by zero.");
		}

		final BigInteger denominator = bigFraction.getDenominator();
		if (BigInteger.ONE.equals(numerator)) {
			return IntegerStruct.valueOf(denominator);
		}
		return RationalStruct.valueOf(denominator, numerator);
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
			final BigFraction rBigFraction = number1.bigFraction;
			final int i = number2.i;
			final BigFraction add = rBigFraction.add(i);
			return RationalStruct.makeRational(add);
		}

		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			final BigFraction rBigFraction = number1.bigFraction;
			final long l = number2.l;
			final BigFraction add = rBigFraction.add(l);
			return RationalStruct.makeRational(add);
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			final BigFraction rBigFraction = number1.bigFraction;
			final BigInteger bigInteger = number2.bigInteger;
			final BigFraction add = rBigFraction.add(bigInteger);
			return RationalStruct.makeRational(add);
		}

		@Override
		public RealStruct add(final SingleFloatStruct number2) {
			final float f1 = number1.bigFraction.floatValue();
			final float f2 = number2.f;
			return FloatStruct.valueOf(f1 + f2);
		}

		@Override
		public RealStruct add(final DoubleFloatStruct number2) {
			final double d1 = number1.bigFraction.doubleValue();
			final double d2 = number2.d;
			return FloatStruct.valueOf(d1 + d2);
		}

		@Override
		public RealStruct add(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigFraction.bigDecimalValue(
					MathContext.DECIMAL128.getPrecision(),
					RoundingMode.HALF_EVEN.ordinal()
			);
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return BigFloatStruct.valueOf(add);
		}

		@Override
		public RealStruct add(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.bigFraction;
			final BigFraction bigFraction2 = number2.bigFraction;
			final BigFraction add = bigFraction1.add(bigFraction2);
			return RationalStruct.makeRational(add);
		}

		@Override
		public NumberStruct add(final ComplexStruct number2) {
			// TODO
			return super.add(number2);
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
			final BigFraction rBigFraction = number1.bigFraction;
			final int i = number2.i;
			final BigFraction subtract = rBigFraction.subtract(i);
			return RationalStruct.makeRational(subtract);
		}

		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			final BigFraction rBigFraction = number1.bigFraction;
			final long l = number2.l;
			final BigFraction subtract = rBigFraction.subtract(l);
			return RationalStruct.makeRational(subtract);
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			final BigFraction rBigFraction = number1.bigFraction;
			final BigInteger bigInteger = number2.bigInteger;
			final BigFraction subtract = rBigFraction.subtract(bigInteger);
			return RationalStruct.makeRational(subtract);
		}

		@Override
		public RealStruct subtract(final SingleFloatStruct number2) {
			final float f1 = number1.bigFraction.floatValue();
			final float f2 = number2.f;
			return FloatStruct.valueOf(f1 - f2);
		}

		@Override
		public RealStruct subtract(final DoubleFloatStruct number2) {
			final double d1 = number1.bigFraction.doubleValue();
			final double d2 = number2.d;
			return FloatStruct.valueOf(d1 - d2);
		}

		@Override
		public RealStruct subtract(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigFraction.bigDecimalValue(
					MathContext.DECIMAL128.getPrecision(),
					RoundingMode.HALF_EVEN.ordinal()
			);
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return FloatStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.bigFraction;
			final BigFraction bigFraction2 = number2.bigFraction;
			final BigFraction subtract = bigFraction1.subtract(bigFraction2);
			return RationalStruct.makeRational(subtract);
		}

		@Override
		public NumberStruct subtract(final ComplexStruct number2) {
			// TODO
			return super.subtract(number2);
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
			final BigFraction rBigFraction = number1.bigFraction;
			final int i = number2.i;
			final BigFraction multiply = rBigFraction.multiply(i);
			return RationalStruct.makeRational(multiply);
		}

		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			final BigFraction rBigFraction = number1.bigFraction;
			final long l = number2.l;
			final BigFraction multiply = rBigFraction.multiply(l);
			return RationalStruct.makeRational(multiply);
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			final BigFraction rBigFraction = number1.bigFraction;
			final BigInteger bigInteger = number2.bigInteger;
			final BigFraction multiply = rBigFraction.multiply(bigInteger);
			return RationalStruct.makeRational(multiply);
		}

		@Override
		public RealStruct multiply(final SingleFloatStruct number2) {
			final float f1 = number1.bigFraction.floatValue();
			final float f2 = number2.f;
			return FloatStruct.valueOf(f1 * f2);
		}

		@Override
		public RealStruct multiply(final DoubleFloatStruct number2) {
			final double d1 = number1.bigFraction.doubleValue();
			final double d2 = number2.d;
			return FloatStruct.valueOf(d1 * d2);
		}

		@Override
		public RealStruct multiply(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigFraction.bigDecimalValue(
					MathContext.DECIMAL128.getPrecision(),
					RoundingMode.HALF_EVEN.ordinal()
			);
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return BigFloatStruct.valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.bigFraction;
			final BigFraction bigFraction2 = number2.bigFraction;
			final BigFraction multiply = bigFraction1.multiply(bigFraction2);
			return RationalStruct.makeRational(multiply);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number2) {
			// TODO
			return super.multiply(number2);
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
			final BigFraction rBigFraction = number1.bigFraction;
			final int i = number2.i;
			final BigFraction divide = rBigFraction.divide(i);
			return RationalStruct.makeRational(divide);
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			final BigFraction rBigFraction = number1.bigFraction;
			final long l = number2.l;
			final BigFraction divide = rBigFraction.divide(l);
			return RationalStruct.makeRational(divide);
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			final BigFraction rBigFraction = number1.bigFraction;
			final BigInteger bigInteger = number2.bigInteger;
			final BigFraction divide = rBigFraction.divide(bigInteger);
			return RationalStruct.makeRational(divide);
		}

		@Override
		public RealStruct divide(final SingleFloatStruct number2) {
			final float f1 = number1.bigFraction.floatValue();
			final float f2 = number2.f;
			return FloatStruct.valueOf(f1 / f2);
		}

		@Override
		public RealStruct divide(final DoubleFloatStruct number2) {
			final double d1 = number1.bigFraction.doubleValue();
			final double d2 = number2.d;
			return FloatStruct.valueOf(d1 / d2);
		}

		@Override
		public RealStruct divide(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigFraction.bigDecimalValue(
					MathContext.DECIMAL128.getPrecision(),
					RoundingMode.HALF_EVEN.ordinal()
			);
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return FloatStruct.valueOf(divide);
		}

		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigFraction bigFraction1 = number1.bigFraction;
			final BigFraction bigFraction2 = number2.bigFraction;
			final BigFraction divide = bigFraction1.divide(bigFraction2);
			return RationalStruct.makeRational(divide);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			// TODO
			return super.divide(number2);
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
		public boolean equalTo(final SingleFloatStruct number2) {
			final BigDecimal bBigDecimal = number1.bigDecimalValue();
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) == 0;
		}

		@Override
		public boolean equalTo(final DoubleFloatStruct number2) {
			final BigDecimal bBigDecimal = number1.bigDecimalValue();
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) == 0;
		}

		@Override
		public boolean equalTo(final BigFloatStruct number2) {
			final BigDecimal bBigDecimal = number1.bigDecimalValue();
			final BigDecimal bigDecimal = number2.bigDecimal;
			return bBigDecimal.compareTo(bigDecimal) == 0;
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
		public boolean lessThan(final SingleFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) < 0;
		}

		@Override
		public boolean lessThan(final DoubleFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) < 0;
		}

		@Override
		public boolean lessThan(final BigFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimal;
			return bBigDecimal.compareTo(bigDecimal) < 0;
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
		public boolean greaterThan(final SingleFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) > 0;
		}

		@Override
		public boolean greaterThan(final DoubleFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) > 0;
		}

		@Override
		public boolean greaterThan(final BigFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimal;
			return bBigDecimal.compareTo(bigDecimal) > 0;
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
		public boolean lessThanOrEqualTo(final SingleFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final DoubleFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final BigFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimal;
			return bBigDecimal.compareTo(bigDecimal) <= 0;
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
		public boolean greaterThanOrEqualTo(final SingleFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final DoubleFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigFloatStruct real2) {
			final BigDecimal bBigDecimal = real1.bigDecimalValue();
			final BigDecimal bigDecimal = real2.bigDecimal;
			return bBigDecimal.compareTo(bigDecimal) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}
	}

	/**
	 * {@link RealStruct.RealExptVisitor} for computing exponential function results for {@link RatioStruct}s.
	 */
	private static final class RatioExptVisitor extends RealStruct.RealExptVisitor<RatioStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerExptVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param base
		 * 		the base argument in the exponential operation
		 */
		private RatioExptVisitor(final RatioStruct base) {
			super(base);
		}

		@Override
		public NumberStruct expt(final IntIntegerStruct power) {
			return super.expt(power);
//			if (power.minusp()) {
//				// TODO: more efficient?
//				return exptInteger(base, power);
//			}
//
//			final BigInteger baseBigInteger = base.bigInteger;
//			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, power.i);
//			return IntegerStruct.valueOf(pow);
		}

		@Override
		public NumberStruct expt(final LongIntegerStruct power) {
			return super.expt(power);
//			if (power.minusp()) {
//				// TODO: more efficient?
//				return exptInteger(base, power);
//			}
//
//			final BigInteger baseBigInteger = base.bigInteger;
//			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, power.l);
//			return IntegerStruct.valueOf(pow);
		}

		@Override
		public NumberStruct expt(final BigIntegerStruct power) {
			return super.expt(power);
//			if (power.minusp()) {
//				// TODO: more efficient?
//				return exptInteger(base, power);
//			}
//
//			final BigInteger baseBigInteger = base.bigInteger;
//			final BigInteger powerBigInteger = power.bigInteger;
//			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, powerBigInteger);
//			return IntegerStruct.valueOf(pow);
		}

		@Override
		public NumberStruct expt(final SingleFloatStruct power) {
			return super.expt(power);
//			// TODO: more efficient?
//			if (LOGGER.isWarnEnabled()) {
//				LOGGER.warn("Possible loss of precision.");
//			}
//			return exptFloatRatioNew(base.bigInteger.doubleValue(), power.f);
		}

		@Override
		public NumberStruct expt(final DoubleFloatStruct power) {
			return super.expt(power);
//			// TODO: more efficient?
//			if (LOGGER.isWarnEnabled()) {
//				LOGGER.warn("Possible loss of precision.");
//			}
//			return exptFloatRatioNew(base.bigInteger.doubleValue(), power.d);
		}

		@Override
		public NumberStruct expt(final BigFloatStruct power) {
			return super.expt(power);
//			// TODO: more efficient?
//			if (LOGGER.isWarnEnabled()) {
//				LOGGER.warn("Possible loss of precision.");
//			}
//			return exptFloatRatioNew(base.bigInteger.doubleValue(), power.doubleValue());
		}

		@Override
		public NumberStruct expt(final RatioStruct power) {
			return super.expt(power);
//			// TODO: more efficient?
//			if (LOGGER.isWarnEnabled()) {
//				LOGGER.warn("Possible loss of precision.");
//			}
//			return exptFloatRatioNew(base.bigInteger.doubleValue(), power.doubleValue());
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
}

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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the object equality of this RatioStruct with the provided {@link LispStruct}. 'EQL' rules state that
	 * if 'x' and 'y' are both numbers of the same type and the same value, then they are equal.
	 */
	@Override
	public boolean lispEql(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the object equality of this RatioStruct with the provided {@link LispStruct}. 'EQUAL' rules state
	 * that if 'x' and 'y' are 'EQL', then they are equal.
	 */
	@Override
	public boolean lispEqual(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the object equality of this RatioStruct with the provided {@link LispStruct}. 'EQUALP' rules state
	 * that if 'x' and 'y' are 'EQL', then they are equal.
	 */
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

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this RatioStruct to a {@link NumberStruct} using the provided {@link AddVisitor}.
	 *
	 * @param addVisitor
	 * 		the {@link AddVisitor} to be used in the addition operation
	 *
	 * @return the addition of {@link NumberStruct} using the provided {@link AddVisitor} and this RatioStruct
	 */
	@Override
	protected NumberStruct add(final AddVisitor<?> addVisitor) {
		return addVisitor.add(this);
	}

	/**
	 * Returns a new {@link AddVisitor} with this RatioStruct to be used in an addition operation.
	 *
	 * @return a new {@link AddVisitor} with this RatioStruct to be used in an addition operation
	 */
	@Override
	protected AddVisitor<?> addVisitor() {
		return new RatioAddVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this RatioStruct to a {@link NumberStruct} using the provided {@link SubtractVisitor}.
	 *
	 * @param subtractVisitor
	 * 		the {@link SubtractVisitor} to be used in the subtraction operation
	 *
	 * @return the subtraction of {@link NumberStruct} using the provided {@link SubtractVisitor} and this RatioStruct
	 */
	@Override
	protected NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	/**
	 * Returns a new {@link SubtractVisitor} with this RatioStruct to be used in a subtraction operation.
	 *
	 * @return a new {@link SubtractVisitor} with this RatioStruct to be used in a subtraction operation
	 */
	@Override
	protected SubtractVisitor<?> subtractVisitor() {
		return new RatioSubtractVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this RatioStruct to a {@link NumberStruct} using the provided {@link MultiplyVisitor}.
	 *
	 * @param multiplyVisitor
	 * 		the {@link MultiplyVisitor} to be used in the multiplication operation
	 *
	 * @return the multiplication of {@link NumberStruct} using the provided {@link MultiplyVisitor} and this
	 * RatioStruct
	 */
	@Override
	protected NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	/**
	 * Returns a new {@link MultiplyVisitor} with this RatioStruct to be used in a multiplication operation.
	 *
	 * @return a new {@link MultiplyVisitor} with this RatioStruct to be used in a multiplication operation
	 */
	@Override
	protected MultiplyVisitor<?> multiplyVisitor() {
		return new RatioMultiplyVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this RatioStruct to a {@link NumberStruct} using the provided {@link DivideVisitor}.
	 *
	 * @param divideVisitor
	 * 		the {@link DivideVisitor} to be used in the division operation
	 *
	 * @return the division of {@link NumberStruct} using the provided {@link DivideVisitor} and this RatioStruct
	 */
	@Override
	protected NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	/**
	 * Returns a new {@link DivideVisitor} with this RatioStruct to be used in a division operation.
	 *
	 * @return a new {@link DivideVisitor} with this RatioStruct to be used in a division operation
	 */
	@Override
	protected DivideVisitor<?> divideVisitor() {
		return new RatioDivideVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this RatioStruct to a {@link NumberStruct} using the provided {@link EqualToVisitor}.
	 *
	 * @param equalToVisitor
	 * 		the {@link EqualToVisitor} to be used in the '=' operation
	 *
	 * @return the '=' comparison of {@link NumberStruct} using the provided {@link EqualToVisitor} and this
	 * RatioStruct
	 */
	@Override
	protected boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	/**
	 * Returns a new {@link EqualToVisitor} with this RatioStruct to be used in an '=' operation.
	 *
	 * @return a new {@link EqualToVisitor} with this RatioStruct to be used in an '=' operation
	 */
	@Override
	protected EqualToVisitor<?> equalToVisitor() {
		return new RatioEqualToVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this RatioStruct to a {@link NumberStruct} using the provided {@link LessThanVisitor}.
	 *
	 * @param lessThanVisitor
	 * 		the {@link LessThanVisitor} to be used in the '<' operation
	 *
	 * @return the '<' comparison of {@link NumberStruct} using the provided {@link LessThanVisitor} and this
	 * RatioStruct
	 */
	@Override
	protected boolean isLessThan(final LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	/**
	 * Returns a new {@link LessThanVisitor} with this RatioStruct to be used in a '<' operation.
	 *
	 * @return a new {@link LessThanVisitor} with this RatioStruct to be used in a '<' operation
	 */
	@Override
	protected LessThanVisitor<?> lessThanVisitor() {
		return new RatioLessThanVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this RatioStruct to a {@link NumberStruct} using the provided {@link GreaterThanVisitor}.
	 *
	 * @param greaterThanVisitor
	 * 		the {@link GreaterThanVisitor} to be used in the '>' operation
	 *
	 * @return the '>' comparison of {@link NumberStruct} using the provided {@link GreaterThanVisitor} and this
	 * RatioStruct
	 */
	@Override
	protected boolean isGreaterThan(final GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	/**
	 * Returns a new {@link GreaterThanVisitor} with this RatioStruct to be used in a '>' operation.
	 *
	 * @return a new {@link GreaterThanVisitor} with this RatioStruct to be used in a '>' operation
	 */
	@Override
	protected GreaterThanVisitor<?> greaterThanVisitor() {
		return new RatioGreaterThanVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this RatioStruct to a {@link NumberStruct} using the provided {@link LessThanOrEqualToVisitor}.
	 *
	 * @param lessThanOrEqualToVisitor
	 * 		the {@link LessThanOrEqualToVisitor} to be used in the '<=' operation
	 *
	 * @return the '<=' comparison of {@link NumberStruct} using the provided {@link LessThanOrEqualToVisitor} and this
	 * RatioStruct
	 */
	@Override
	protected boolean isLessThanOrEqualTo(final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	/**
	 * Returns a new {@link LessThanOrEqualToVisitor} with this RatioStruct to be used in a '<=' operation.
	 *
	 * @return a new {@link LessThanOrEqualToVisitor} with this RatioStruct to be used in a '<=' operation
	 */
	@Override
	protected LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new RatioLessThanOrEqualToVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this RatioStruct to a {@link NumberStruct} using the provided {@link GreaterThanOrEqualToVisitor}.
	 *
	 * @param greaterThanOrEqualToVisitor
	 * 		the {@link GreaterThanOrEqualToVisitor} to be used in the '>=' operation
	 *
	 * @return the '>=' comparison of {@link NumberStruct} using the provided {@link GreaterThanOrEqualToVisitor} and
	 * this RatioStruct
	 */
	@Override
	protected boolean isGreaterThanOrEqualTo(final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	/**
	 * Returns a new {@link GreaterThanOrEqualToVisitor} with this RatioStruct to be used in a '>=' operation.
	 *
	 * @return a new {@link GreaterThanOrEqualToVisitor} with this RatioStruct to be used in a '>=' operation
	 */
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

	/**
	 * {@inheritDoc}
	 * <p>
	 * Performs the exponential operation with this RatioStruct as the power value using the provided {@link
	 * ExptVisitor}.
	 *
	 * @param exptVisitor
	 * 		the {@link ExptVisitor} to be used in the exponential operation
	 *
	 * @return the result of the exponential operation with this RatioStruct as the power value using the provided
	 * {@link ExptVisitor}
	 */
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
		} catch (final ArithmeticException ae) {
			final Logger logger = LoggerFactory.getLogger(RatioStruct.class);
			if (logger.isTraceEnabled()) {
				logger.trace(ae.getMessage(), ae);
			}
			// This means that we have to round the fraction.
			final int scale = MathContext.DECIMAL128.getPrecision();
			final int roundingMode = RoundingMode.HALF_EVEN.ordinal();
			return bigFraction.bigDecimalValue(scale, roundingMode);
		}
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

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'FLOOR' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'FLOOR' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'FLOOR' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	protected QuotientRemainderResult floor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.floor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'FLOOR' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}. The resulting 'quotient' will be a {@link FloatStruct}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'FLOOR' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'FLOOR' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	protected QuotientRemainderResult ffloor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ffloor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'CEILING' operation with this RatioStruct as the
	 * 'divisor' using the provided {@link QuotientRemainderVisitor}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'CEILING' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'CEILING' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	protected QuotientRemainderResult ceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ceiling(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'CEILING' operation with this RatioStruct as the
	 * 'divisor' using the provided {@link QuotientRemainderVisitor}. The resulting 'quotient' will be a {@link
	 * FloatStruct}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'CEILING' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'CEILING' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	protected QuotientRemainderResult fceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fceiling(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'ROUND' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'ROUND' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'ROUND' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	protected QuotientRemainderResult round(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.round(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'ROUND' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}. The resulting 'quotient' will be a {@link FloatStruct}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'ROUND' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'ROUND' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	protected QuotientRemainderResult fround(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fround(this);
	}

	/**
	 * Returns a new {@link QuotientRemainderVisitor} with this RatioStruct to be used in a 'quotient' and 'remainder'
	 * calculation operation.
	 *
	 * @return a new {@link QuotientRemainderVisitor} with this RatioStruct to be used in a 'quotient' and 'remainder'
	 * calculation operation
	 */
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

	// HashCode / Equals

	/**
	 * Returns a hash code for this object using a {@link HashCodeBuilder}.
	 *
	 * @return a hash code for this object
	 */
	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bigFraction)
		                            .toHashCode();
	}

	/**
	 * Returns the Java object equality of this object using an {@link EqualsBuilder}. If the provided {@code obj} is
	 * null, it is not equal. If the provided {@code obj} is '==' to {@code this}, it is equal. If the {@link Class} of
	 * the provided {@code obj} is not equal to {@link #getClass()}, it is not equal.
	 *
	 * @param obj
	 * 		the {@link Object} to tests for Java object equality
	 *
	 * @return true if the objects are equal; false otherwise
	 */
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

	// Visitor Implementations

	/**
	 * {@link RealAddVisitor} for computing addition results for {@link RatioStruct}s.
	 */
	private static final class RatioAddVisitor extends RealAddVisitor<RatioStruct> {

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
		 * Package private constructor to make a new instance of an RatioSubtractVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		RatioSubtractVisitor(final RatioStruct number1) {
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
		 * Package private constructor to make a new instance of an RatioMultiplyVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		RatioMultiplyVisitor(final RatioStruct number1) {
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
		 * Package private constructor to make a new instance of an RatioDivideVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		RatioDivideVisitor(final RatioStruct number1) {
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
	 * {@link RealEqualToVisitor} for computing numeric '=' equality results for {@link RatioStruct}s.
	 */
	private static final class RatioEqualToVisitor extends RealEqualToVisitor<RatioStruct> {

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
		 * Package private constructor to make a new instance of an RatioLessThanVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric '<' equality operation
		 */
		RatioLessThanVisitor(final RatioStruct real1) {
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
		 * Package private constructor to make a new instance of an RatioGreaterThanVisitor with the provided {@link
		 * RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric '>' equality operation
		 */
		RatioGreaterThanVisitor(final RatioStruct real1) {
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
		 * Package private constructor to make a new instance of an RatioLessThanOrEqualToVisitor with the provided
		 * {@link RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric '<=' equality operation
		 */
		RatioLessThanOrEqualToVisitor(final RatioStruct real1) {
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
		 * Package private constructor to make a new instance of an RatioGreaterThanOrEqualToVisitor with the provided
		 * {@link RatioStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric '>=' equality operation
		 */
		RatioGreaterThanOrEqualToVisitor(final RatioStruct real1) {
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
		 * Package private constructor to make a new instance of an RatioQuotientRemainderVisitor with the provided
		 * {@link RatioStruct}.
		 *
		 * @param real
		 * 		the real argument in the computational quotient and remainder operation
		 */
		RatioQuotientRemainderVisitor(final RatioStruct real) {
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
}

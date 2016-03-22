/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import jcl.classes.BuiltInClassStruct;
import jcl.types.IntegerType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;

/**
 * The {@link LongIntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public class LongIntegerStruct extends BuiltInClassStruct implements IntegerStruct {

	/**
	 * {@link LongIntegerStruct} constant representing 0.
	 */
	public static final LongIntegerStruct ZERO = new LongIntegerStruct(BigInteger.ZERO);

	/**
	 * {@link LongIntegerStruct} constant representing 1.
	 */
	public static final LongIntegerStruct ONE = new LongIntegerStruct(BigInteger.ONE);

	/**
	 * {@link LongIntegerStruct} constant representing 2.
	 */
	public static final LongIntegerStruct TWO = new LongIntegerStruct(BigInteger.valueOf(2));

	/**
	 * {@link LongIntegerStruct} constant representing 10.
	 */
	public static final LongIntegerStruct TEN = new LongIntegerStruct(BigInteger.TEN);

	/**
	 * {@link LongIntegerStruct} constant representing -1.
	 */
	public static final LongIntegerStruct MINUS_ONE = new LongIntegerStruct(BigInteger.valueOf(-1));

	/**
	 * The internal {@link BigInteger} containing the float contents.
	 */
	private final BigInteger bigInteger;

	/**
	 * Public constructor.
	 *
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public LongIntegerStruct(final BigInteger bigInteger) {
		this(IntegerType.INSTANCE, bigInteger);
	}

	/**
	 * Public constructor.
	 *
	 * @param integerType
	 * 		a {@link IntegerType} that represents the type of {@link IntegerType}
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public LongIntegerStruct(final IntegerType integerType, final BigInteger bigInteger) {
		super(integerType, null, null);
		this.bigInteger = bigInteger;
	}

	/**
	 * Public constructor.
	 *
	 * @param apint
	 * 		the value of the IntegerStruct
	 */
	LongIntegerStruct(final Apint apint) {
		this(apint.toBigInteger());
	}

	/**
	 * Public constructor.
	 *
	 * @param apfloat
	 * 		the value of the IntegerStruct
	 */
	LongIntegerStruct(final Apfloat apfloat) {
		this(apfloat.longValue());
	}

	/**
	 * Public constructor.
	 *
	 * @param longValue
	 * 		the value of the IntegerStruct
	 */
	private LongIntegerStruct(final long longValue) {
		this(BigInteger.valueOf(longValue));
	}

	/**
	 * Getter for integer {@link #bigInteger} property.
	 *
	 * @return integer {@link #bigInteger} property
	 */
	public BigInteger getBigInteger() {
		return bigInteger;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the absolute value of this IntegerStruct.
	 */
	@Override
	public RealStruct abs() {
		if (bigInteger.signum() >= 0) {
			return this;
		}
		final BigInteger negate = bigInteger.negate();
		return new LongIntegerStruct(negate);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this IntegerStruct is zero using {@link BigInteger#signum()} on {@link #bigInteger}.
	 */
	@Override
	public boolean zerop() {
		return bigInteger.signum() == 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this IntegerStruct is positive using {@link BigInteger#signum()} on {@link
	 * #bigInteger}.
	 */
	@Override
	public boolean plusp() {
		return bigInteger.signum() > 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this IntegerStruct is negative using {@link BigInteger#signum()} on {@link
	 * #bigInteger}.
	 */
	@Override
	public boolean minusp() {
		return bigInteger.signum() < 0;
	}

	/**
	 * Returns true if this IntegerStruct is even (divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct is even (divisible by two); otherwise, false
	 */
	public boolean evenp() {
		return !bigInteger.testBit(0);
	}

	/**
	 * Returns true if this IntegerStruct is odd (not divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct is odd (not divisible by two); otherwise, false
	 */
	public boolean oddp() {
		return bigInteger.testBit(0);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this IntegerStruct to a {@link NumberStruct} using the provided {@link AddVisitor}.
	 *
	 * @param addVisitor
	 * 		the {@link AddVisitor} to be used in the addition operation
	 *
	 * @return the addition of {@link NumberStruct} using the provided {@link AddVisitor} and this IntegerStruct
	 */
	@Override
	public NumberStruct add(final AddVisitor<?> addVisitor) {
		return addVisitor.add(this);
	}

	/**
	 * Returns a new {@link AddVisitor} with this IntegerStruct to be used in an addition operation.
	 *
	 * @return a new {@link AddVisitor} with this IntegerStruct to be used in an addition operation
	 */
	@Override
	public AddVisitor<?> addVisitor() {
		return new IntegerAddVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this IntegerStruct to a {@link NumberStruct} using the provided {@link SubtractVisitor}.
	 *
	 * @param subtractVisitor
	 * 		the {@link SubtractVisitor} to be used in the subtraction operation
	 *
	 * @return the subtraction of {@link NumberStruct} using the provided {@link SubtractVisitor} and this IntegerStruct
	 */
	@Override
	public NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	/**
	 * Returns a new {@link SubtractVisitor} with this IntegerStruct to be used in a subtraction operation.
	 *
	 * @return a new {@link SubtractVisitor} with this IntegerStruct to be used in a subtraction operation
	 */
	@Override
	public SubtractVisitor<?> subtractVisitor() {
		return new IntegerSubtractVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this IntegerStruct to a {@link NumberStruct} using the provided {@link MultiplyVisitor}.
	 *
	 * @param multiplyVisitor
	 * 		the {@link MultiplyVisitor} to be used in the multiplication operation
	 *
	 * @return the multiplication of {@link NumberStruct} using the provided {@link MultiplyVisitor} and this
	 * IntegerStruct
	 */
	@Override
	public NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	/**
	 * Returns a new {@link MultiplyVisitor} with this IntegerStruct to be used in a multiplication operation.
	 *
	 * @return a new {@link MultiplyVisitor} with this IntegerStruct to be used in a multiplication operation
	 */
	@Override
	public MultiplyVisitor<?> multiplyVisitor() {
		return new IntegerMultiplyVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this IntegerStruct to a {@link NumberStruct} using the provided {@link DivideVisitor}.
	 *
	 * @param divideVisitor
	 * 		the {@link DivideVisitor} to be used in the division operation
	 *
	 * @return the division of {@link NumberStruct} using the provided {@link DivideVisitor} and this IntegerStruct
	 */
	@Override
	public NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	/**
	 * Returns a new {@link DivideVisitor} with this IntegerStruct to be used in a division operation.
	 *
	 * @return a new {@link DivideVisitor} with this IntegerStruct to be used in a division operation
	 */
	@Override
	public DivideVisitor<?> divideVisitor() {
		return new IntegerDivideVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this IntegerStruct to a {@link NumberStruct} using the provided {@link EqualToVisitor}.
	 *
	 * @param equalToVisitor
	 * 		the {@link EqualToVisitor} to be used in the '=' operation
	 *
	 * @return the '=' comparison of {@link NumberStruct} using the provided {@link EqualToVisitor} and this
	 * IntegerStruct
	 */
	@Override
	public boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	/**
	 * Returns a new {@link EqualToVisitor} with this IntegerStruct to be used in an '=' operation.
	 *
	 * @return a new {@link EqualToVisitor} with this IntegerStruct to be used in an '=' operation
	 */
	@Override
	public EqualToVisitor<?> equalToVisitor() {
		return new IntegerEqualToVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this IntegerStruct to a {@link NumberStruct} using the provided {@link LessThanVisitor}.
	 *
	 * @param lessThanVisitor
	 * 		the {@link LessThanVisitor} to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of {@link NumberStruct} using the provided {@link
	 * LessThanVisitor} and this
	 * IntegerStruct
	 */
	@Override
	public boolean isLessThan(final LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	/**
	 * Returns a new {@link LessThanVisitor} with this IntegerStruct to be used in a {@literal '<'}
	 * operation.
	 *
	 * @return a new {@link LessThanVisitor} with this IntegerStruct to be used in a {@literal '<'} operation
	 */
	@Override
	public LessThanVisitor<?> lessThanVisitor() {
		return new IntegerLessThanVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this IntegerStruct to a {@link NumberStruct} using the provided {@link GreaterThanVisitor}.
	 *
	 * @param greaterThanVisitor
	 * 		the {@link GreaterThanVisitor} to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of {@link NumberStruct} using the provided {@link
	 * GreaterThanVisitor} and
	 * this IntegerStruct
	 */
	@Override
	public boolean isGreaterThan(final GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	/**
	 * Returns a new {@link GreaterThanVisitor} with this IntegerStruct to be used in a {@literal '>'}
	 * operation.
	 *
	 * @return a new {@link GreaterThanVisitor} with this IntegerStruct to be used in a {@literal '>'}
	 * operation
	 */
	@Override
	public GreaterThanVisitor<?> greaterThanVisitor() {
		return new IntegerGreaterThanVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this IntegerStruct to a {@link NumberStruct} using the provided {@link
	 * LessThanOrEqualToVisitor}.
	 *
	 * @param lessThanOrEqualToVisitor
	 * 		the {@link LessThanOrEqualToVisitor} to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of {@link NumberStruct} using the provided {@link
	 * LessThanOrEqualToVisitor} and this IntegerStruct
	 */
	@Override
	public boolean isLessThanOrEqualTo(final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	/**
	 * Returns a new {@link LessThanOrEqualToVisitor} with this IntegerStruct to be used in a {@literal
	 * '<='}
	 * operation.
	 *
	 * @return a new {@link LessThanOrEqualToVisitor} with this IntegerStruct to be used in a {@literal '<='}
	 * operation
	 */
	@Override
	public LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new IntegerLessThanOrEqualToVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this IntegerStruct to a {@link NumberStruct} using the provided {@link
	 * GreaterThanOrEqualToVisitor}.
	 *
	 * @param greaterThanOrEqualToVisitor
	 * 		the {@link GreaterThanOrEqualToVisitor} to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of {@link NumberStruct} using the provided {@link
	 * GreaterThanOrEqualToVisitor} and this IntegerStruct
	 */
	@Override
	public boolean isGreaterThanOrEqualTo(final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	/**
	 * Returns a new {@link GreaterThanOrEqualToVisitor} with this IntegerStruct to be used in a {@literal
	 * '>='}
	 * operation.
	 *
	 * @return a new {@link GreaterThanOrEqualToVisitor} with this IntegerStruct to be used in a {@literal
	 * '>='}
	 * operation
	 */
	@Override
	public GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new IntegerGreaterThanOrEqualToVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the whether or not the numerical value of this IntegerStruct is zero, positive, or negative,
	 * returning {@code this}, {@link #ONE}, or {@link #MINUS_ONE} respectively.
	 */
	@Override
	public NumberStruct signum() {
		if (zerop()) {
			return this;
		} else if (plusp()) {
			return ONE;
		} else {
			return MINUS_ONE;
		}
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #ZERO} as the imaginary part of IntegerStructs is always '0'.
	 */
	@Override
	public RealStruct imagPart() {
		return ZERO;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the negation with {@link BigInteger#negate()} on {@link #bigInteger} and the creating a new
	 * IntegerStruct to wrap it.
	 */
	@Override
	public NumberStruct negation() {
		final BigInteger negate = bigInteger.negate();
		return new LongIntegerStruct(negate);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Creates a new {@link RationalStruct} with {@link BigInteger#ONE} as the numerator and {@link #bigInteger} as the
	 * denominator.
	 */
	@Override
	public NumberStruct reciprocal() {
		return RationalStruct.makeRational(BigInteger.ONE, bigInteger);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the exponential function result for this IntegerStruct as this {@code base} and the provided {@link
	 * NumberStruct} as the {@code power}. If {@code power} is '0' and power is an IntegerStruct, {@link #ONE} is
	 * returned. If {@code power} is '0' and power is not an IntegerStruct, {@link FloatStruct#ONE} is returned. If
	 * this IntegerStruct is either '0' or '1', {@code this} is returned.
	 */
	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			if (power instanceof LongIntegerStruct) {
				return ONE;
			}
			return FloatStruct.ONE;
		}

		if (zerop() || isEqualTo(ONE)) {
			return this;
		}

		final ExptVisitor<?> exptVisitor = exptVisitor();
		return power.expt(exptVisitor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Performs the exponential operation with this IntegerStruct as the power value using the provided {@link
	 * ExptVisitor}.
	 *
	 * @param exptVisitor
	 * 		the {@link ExptVisitor} to be used in the exponential operation
	 *
	 * @return the result of the exponential operation with this IntegerStruct as the power value using the provided
	 * {@link ExptVisitor}
	 */
	@Override
	public NumberStruct expt(final ExptVisitor<?> exptVisitor) {
		return exptVisitor.expt(this);
	}

	/**
	 * Returns a new {@link IntegerExptVisitor} with this IntegerStruct to be used in an exponential operation.
	 *
	 * @return a new {@link IntegerExptVisitor} with this IntegerStruct to be used in an exponential operation
	 */
	@Override
	public ExptVisitor<?> exptVisitor() {
		return new IntegerExptVisitor(this);
	}

	/**
	 * Returns the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root.
	 *
	 * @return the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root
	 */
	public LongIntegerStruct isqrt() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat sqrt = ApfloatMath.sqrt(apfloat);
		final Apint floor = sqrt.floor();
		final BigInteger floorBigInteger = floor.toBigInteger();
		return new LongIntegerStruct(floorBigInteger);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes a {@link BigDecimal} value from the {@link #bigInteger} by altering its scale when creating a new
	 * {@link BigDecimal} and then further multiplying it by {@link BigDecimal#TEN}.
	 */
	@Override
	public BigDecimal bigDecimalValue() {
		return new BigDecimal(bigInteger, 1).multiply(BigDecimal.TEN);
	}

	@Override
	public Apfloat apfloatValue() {
		return new Apint(bigInteger);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #ZERO} as a '0' IntegerStruct value.
	 */
	@Override
	public RealStruct zeroValue() {
		return ZERO;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'FLOOR' operation with this IntegerStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'FLOOR' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'FLOOR' operation with this IntegerStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	public QuotientRemainderResult floor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.floor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'FLOOR' operation with this IntegerStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}. The resulting 'quotient' will be a {@link FloatStruct}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'FLOOR' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'FLOOR' operation with this IntegerStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	public QuotientRemainderResult ffloor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ffloor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'CEILING' operation with this IntegerStruct as the
	 * 'divisor' using the provided {@link QuotientRemainderVisitor}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'CEILING' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'CEILING' operation with this IntegerStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	public QuotientRemainderResult ceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ceiling(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'CEILING' operation with this IntegerStruct as the
	 * 'divisor' using the provided {@link QuotientRemainderVisitor}. The resulting 'quotient' will be a {@link
	 * FloatStruct}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'CEILING' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'CEILING' operation with this IntegerStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	public QuotientRemainderResult fceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fceiling(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'ROUND' operation with this IntegerStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'ROUND' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'ROUND' operation with this IntegerStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	public QuotientRemainderResult round(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.round(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'ROUND' operation with this IntegerStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}. The resulting 'quotient' will be a {@link FloatStruct}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'ROUND' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'ROUND' operation with this IntegerStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	@Override
	public QuotientRemainderResult fround(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fround(this);
	}

	/**
	 * Returns a new {@link QuotientRemainderVisitor} with this IntegerStruct to be used in a 'quotient' and
	 * 'remainder' calculation operation.
	 *
	 * @return a new {@link QuotientRemainderVisitor} with this IntegerStruct to be used in a 'quotient' and 'remainder'
	 * calculation operation
	 */
	@Override
	public QuotientRemainderVisitor<?> quotientRemainderVisitor() {
		return new IntegerQuotientRemainderVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@code this} as the numerator.
	 */
	@Override
	public LongIntegerStruct numerator() {
		return this;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #ONE} as the denominator of IntegerStructs is always '1'.
	 */
	@Override
	public LongIntegerStruct denominator() {
		return ONE;
	}

	/**
	 * Returns the greatest common divisor between this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct in comparison to this IntegerStruct to determine the greatest common divisor
	 *
	 * @return the greatest common divisor between this IntegerStruct and the provided IntegerStruct
	 */
	public LongIntegerStruct gcd(final IntegerStruct integer) {
		final BigInteger gcd = bigInteger.gcd(integer.getBigInteger());
		return new LongIntegerStruct(gcd);
	}

	/**
	 * Returns the least common multiple between this IntegerStruct and the provided IntegerStruct. If this or the
	 * provided IntegerStruct are '0', the result is {@link #ZERO}.
	 *
	 * @param integer
	 * 		the IntegerStruct in comparison to this IntegerStruct to determine the least common multiple
	 *
	 * @return the least common multiple between this IntegerStruct and the provided IntegerStruct
	 */
	public LongIntegerStruct lcm(final IntegerStruct integer) {
		if (zerop() || integer.zerop()) {
			return ZERO;
		}

		// lcm(x y) = abs(x * y) / gcd(x y)
		final BigInteger multiply = bigInteger.multiply(integer.getBigInteger());
		final BigInteger abs = multiply.abs();
		final BigInteger gcd = bigInteger.gcd(integer.getBigInteger());
		final BigInteger divide = abs.divide(gcd);

		return new LongIntegerStruct(divide);
	}

	/**
	 * Performs the arithmetic shift operation on the binary representation of this IntegerStruct, shifting the bits
	 * left or right by the provided {@code count} IntegerStruct based on its sign. If the {@code count} value is '0',
	 * the result is {@code this}.
	 *
	 * @param count
	 * 		the bit positions to shift this IntegerStruct left or right.
	 *
	 * @return the arithmetic shift operation on the binary representation of this IntegerStruct
	 */
	public LongIntegerStruct ash(final IntegerStruct count) {
		if (count.zerop()) {
			return this;
		}

		final int countInt = count.getBigInteger().intValue();

		// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
		final BigInteger shiftLeft = bigInteger.shiftLeft(countInt);
		return new LongIntegerStruct(shiftLeft);
	}

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public LongIntegerStruct logAnd(final IntegerStruct integer) {
		final BigInteger and = bigInteger.and(integer.getBigInteger());
		return new LongIntegerStruct(and);
	}

	/**
	 * Returns the bit-wise logical 'and' of the compliment of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public LongIntegerStruct logAndC1(final IntegerStruct integer) {
		final BigInteger not = bigInteger.not();
		final BigInteger and = not.and(integer.getBigInteger());
		return new LongIntegerStruct(and);
	}

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public LongIntegerStruct logAndC2(final IntegerStruct integer) {
		final BigInteger not = integer.getBigInteger().not();
		final BigInteger and = bigInteger.and(not);
		return new LongIntegerStruct(and);
	}

	/**
	 * Returns the bit-wise logical 'equivalence', or 'exclusive-nor' of this IntegerStruct and the provided
	 * IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'equivalence', or 'exclusive-nor' of this IntegerStruct and the provided
	 * IntegerStruct
	 */
	public LongIntegerStruct logEqv(final IntegerStruct integer) {
		final BigInteger xor = bigInteger.xor(integer.getBigInteger());
		final BigInteger not = xor.not();
		return new LongIntegerStruct(not);
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	public LongIntegerStruct logIor(final IntegerStruct integer) {
		final BigInteger or = bigInteger.or(integer.getBigInteger());
		return new LongIntegerStruct(or);
	}

	/**
	 * Returns the bit-wise logical compliment 'and' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical compliment 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public LongIntegerStruct logNand(final IntegerStruct integer) {
		final BigInteger and = bigInteger.and(integer.getBigInteger());
		final BigInteger not = and.not();
		return new LongIntegerStruct(not);
	}

	/**
	 * Returns the bit-wise logical compliment 'or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical compliment 'or' of this IntegerStruct and the provided IntegerStruct
	 */
	public LongIntegerStruct logNor(final IntegerStruct integer) {
		final BigInteger or = bigInteger.or(integer.getBigInteger());
		final BigInteger not = or.not();
		return new LongIntegerStruct(not);
	}

	/**
	 * Returns the bit-wise logical 'not' of this IntegerStruct.
	 *
	 * @return the bit-wise logical 'not' of this IntegerStruct
	 */
	public LongIntegerStruct logNot() {
		final BigInteger not = bigInteger.not();
		return new LongIntegerStruct(not);
	}

	/**
	 * Returns the bit-wise logical 'or' of the compliment of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'or' of the compliment of this IntegerStruct and the provided IntegerStruct
	 */
	public LongIntegerStruct logOrC1(final IntegerStruct integer) {
		final BigInteger not = bigInteger.not();
		final BigInteger or = not.or(integer.getBigInteger());
		return new LongIntegerStruct(or);
	}

	/**
	 * Returns the bit-wise logical 'or' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'or' of this IntegerStruct and the compliment of provided IntegerStruct
	 */
	public LongIntegerStruct logOrC2(final IntegerStruct integer) {
		final BigInteger not = integer.getBigInteger().not();
		final BigInteger or = bigInteger.or(not);
		return new LongIntegerStruct(or);
	}

	/**
	 * Returns the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	public LongIntegerStruct logXor(final IntegerStruct integer) {
		final BigInteger xor = bigInteger.xor(integer.getBigInteger());
		return new LongIntegerStruct(xor);
	}

	/**
	 * Returns the number of bits needed to represent this IntegerStruct in binary two's-complement format.
	 *
	 * @return the number of bits needed to represent this IntegerStruct in binary two's-complement format
	 */
	public LongIntegerStruct integerLength() {
		final int bitLength = bigInteger.bitLength();
		final BigInteger bitLengthBigInteger = BigInteger.valueOf(bitLength);
		return new LongIntegerStruct(bitLengthBigInteger);
	}

	/**
	 * Returns true if the bit in this IntegerStruct whose index is {@code index} is a one-bit; otherwise, returns
	 * false.
	 *
	 * @param index
	 * 		the index value to test this IntegerStruct for a one-bit
	 *
	 * @return true if the bit in this IntegerStruct whose index is {@code index} is a one-bit; otherwise, false
	 */
	public boolean logBitP(final IntegerStruct index) {
		final int indexInt = index.getBigInteger().intValue();
		return bigInteger.testBit(indexInt);
	}

	/**
	 * Computes and returns the number of bits in the two's-complement binary representation of this IntegerStruct that
	 * are 'on' or 'set'. If this IntegerStruct is negative, the 0 bits are counted; otherwise, the 1 bits are counted.
	 *
	 * @return Computes and returns the number of bits in the two's-complement binary representation of this
	 * IntegerStruct that are 'on' or 'set'
	 */
	public LongIntegerStruct logCount() {
		final int bitCount = bigInteger.bitCount();
		final BigInteger bitCountBigInteger = BigInteger.valueOf(bitCount);
		return new LongIntegerStruct(bitCountBigInteger);
	}

	/**
	 * Returns true if any of the bits designated by the 1's in this IntegerStruct are 1 in the provided IntegerStruct;
	 * otherwise, returns false.
	 *
	 * @param integer
	 * 		the IntegerStruct used in the test comparison to this IntegerStruct
	 *
	 * @return true if any of the bits designated by the 1's in this IntegerStruct are 1 in the provided IntegerStruct;
	 * otherwise, false.
	 */
	public boolean logTest(final IntegerStruct integer) {
		final BigInteger and = bigInteger.and(integer.getBigInteger());
		return and.signum() != 0;
	}

	// Comparison Visitor Helpers

	/**
	 * Determines numeric comparison result between the provided IntegerStructs.
	 *
	 * @param number1
	 * 		the first IntegerStruct in the comparison operation
	 * @param number2
	 * 		the second IntegerStruct in the comparison operation
	 *
	 * @return numeric comparison result between the provided IntegerStructs
	 */
	private static int getComparisonResult(final LongIntegerStruct number1, final LongIntegerStruct number2) {
		final BigInteger bigInteger1 = number1.bigInteger;
		final BigInteger bigInteger2 = number2.bigInteger;
		return bigInteger1.compareTo(bigInteger2);
	}

	/**
	 * Determines numeric comparison result between the provided IntegerStruct and {@link RatioStruct}.
	 *
	 * @param number1
	 * 		the IntegerStruct in the comparison operation
	 * @param number2
	 * 		the {@link RatioStruct} in the comparison operation
	 *
	 * @return numeric comparison result between the provided IntegerStruct and {@link RatioStruct}
	 */
	private static int getComparisonResult(final LongIntegerStruct number1, final RatioStruct number2) {
		final BigInteger bigInteger1 = number1.bigInteger;

		final BigFraction bigFraction2 = number2.getBigFraction();
		final BigFraction bigFraction2Reduced = bigFraction2.reduce();
		final BigInteger numerator = bigFraction2Reduced.getNumerator();
		final BigInteger denominator = bigFraction2Reduced.getDenominator();

		final BigInteger multiply = bigInteger1.multiply(denominator);
		return multiply.compareTo(numerator);
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
		                            .append(bigInteger)
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
		final LongIntegerStruct rhs = (LongIntegerStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigInteger, rhs.bigInteger)
		                          .isEquals();
	}

	// Visitor Implementations

	/**
	 * {@link RealAddVisitor} for computing addition results for {@link LongIntegerStruct}s.
	 */
	private static final class IntegerAddVisitor extends RealAddVisitor<LongIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerAddVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		IntegerAddVisitor(final LongIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct add(final IntIntegerStruct number2) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for {@link LongIntegerStruct}s.
		 */
		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger add = bigInteger1.add(bigInteger2);
			return new LongIntegerStruct(add);
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for an {@link LongIntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct add(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			final BigInteger add = multiply.add(numerator);
			return RationalStruct.makeRational(add, denominator);
		}
	}

	/**
	 * {@link RealSubtractVisitor} for computing subtraction function results for {@link LongIntegerStruct}s.
	 */
	private static final class IntegerSubtractVisitor extends RealSubtractVisitor<LongIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerSubtractVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		IntegerSubtractVisitor(final LongIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct subtract(final IntIntegerStruct number2) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for {@link LongIntegerStruct}s.
		 */
		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger subtract = bigInteger1.subtract(bigInteger2);
			return new LongIntegerStruct(subtract);
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for an {@link LongIntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			final BigInteger subtract = multiply.subtract(numerator);
			return RationalStruct.makeRational(subtract, denominator);
		}
	}

	/**
	 * {@link RealMultiplyVisitor} for computing multiplication function results for {@link LongIntegerStruct}s.
	 */
	private static final class IntegerMultiplyVisitor extends RealMultiplyVisitor<LongIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerMultiplyVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		IntegerMultiplyVisitor(final LongIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct multiply(final IntIntegerStruct number2) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for {@link LongIntegerStruct}s.
		 */
		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger multiply = bigInteger1.multiply(bigInteger2);
			return new LongIntegerStruct(multiply);
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for an {@link LongIntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(numerator);
			return RationalStruct.makeRational(multiply, denominator);
		}
	}

	/**
	 * {@link RealDivideVisitor} for computing division function results for {@link LongIntegerStruct}s.
	 */
	private static final class IntegerDivideVisitor extends RealDivideVisitor<LongIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerDivideVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		IntegerDivideVisitor(final LongIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct divide(final IntIntegerStruct number2) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for {@link LongIntegerStruct}s.
		 */
		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			return RationalStruct.makeRational(bigInteger1, bigInteger2);
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for an {@link LongIntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			return RationalStruct.makeRational(multiply, numerator);
		}
	}

	/**
	 * {@link RealEqualToVisitor} for computing numeric '=' equality results for {@link LongIntegerStruct}s.
	 */
	private static final class IntegerEqualToVisitor extends RealEqualToVisitor<LongIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerEqualToVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		IntegerEqualToVisitor(final LongIntegerStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '=' equality result for {@link LongIntegerStruct}s.
		 */
		@Override
		public boolean equalTo(final LongIntegerStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '=' equality result for an {@link LongIntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public boolean equalTo(final RatioStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}
	}

	/**
	 * {@link LessThanVisitor} for computing numeric {@literal '<'} equality results for {@link
	 * LongIntegerStruct}s.
	 */
	private static final class IntegerLessThanVisitor extends LessThanVisitor<LongIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerLessThanVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		IntegerLessThanVisitor(final LongIntegerStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<'} equality result for {@link LongIntegerStruct}s.
		 */
		@Override
		public boolean lessThan(final LongIntegerStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<'} equality result for an {@link LongIntegerStruct} and a {@link
		 * RatioStruct}.
		 */
		@Override
		public boolean lessThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}
	}

	/**
	 * {@link GreaterThanVisitor} for computing numeric {@literal '>'} equality results for {@link
	 * LongIntegerStruct}s.
	 */
	private static final class IntegerGreaterThanVisitor extends GreaterThanVisitor<LongIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerGreaterThanVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		IntegerGreaterThanVisitor(final LongIntegerStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>'} equality result for {@link LongIntegerStruct}s.
		 */
		@Override
		public boolean greaterThan(final LongIntegerStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>'} equality result for an {@link LongIntegerStruct} and a {@link
		 * RatioStruct}.
		 */
		@Override
		public boolean greaterThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}
	}

	/**
	 * {@link LessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality results for {@link
	 * LongIntegerStruct}s.
	 */
	private static final class IntegerLessThanOrEqualToVisitor extends LessThanOrEqualToVisitor<LongIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerLessThanOrEqualToVisitor with the provided
		 * {@link LongIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		IntegerLessThanOrEqualToVisitor(final LongIntegerStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<='} equality result for {@link LongIntegerStruct}s.
		 */
		@Override
		public boolean lessThanOrEqualTo(final LongIntegerStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<='} equality result for an {@link LongIntegerStruct} and a {@link
		 * RatioStruct}.
		 */
		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}
	}

	/**
	 * {@link GreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality results for {@link
	 * LongIntegerStruct}s.
	 */
	private static final class IntegerGreaterThanOrEqualToVisitor extends GreaterThanOrEqualToVisitor<LongIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerGreaterThanOrEqualToVisitor with the
		 * provided {@link LongIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		IntegerGreaterThanOrEqualToVisitor(final LongIntegerStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>='} equality result for {@link LongIntegerStruct}s.
		 */
		@Override
		public boolean greaterThanOrEqualTo(final LongIntegerStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>='} equality result for an {@link LongIntegerStruct} and a {@link
		 * RatioStruct}.
		 */
		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}
	}

	/**
	 * {@link IntegerQuotientRemainderVisitor} for computing quotient and remainder results for {@link
	 * LongIntegerStruct}s.
	 */
	private static final class IntegerQuotientRemainderVisitor extends RationalQuotientRemainderVisitor<LongIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerQuotientRemainderVisitor with the provided
		 * {@link LongIntegerStruct}.
		 *
		 * @param real
		 * 		the real argument in the computational quotient and remainder operation
		 */
		IntegerQuotientRemainderVisitor(final LongIntegerStruct real) {
			super(real);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final IntIntegerStruct divisor, final RoundingMode roundingMode, final boolean isQuotientFloat) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the quotient and remainder results for {@link LongIntegerStruct}s as the {@code real} and {@code
		 * divisor}.
		 */
		@Override
		public QuotientRemainderResult quotientRemainder(final LongIntegerStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {

			final BigDecimal realBigDecimal = real.bigDecimalValue();
			final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();

			final BigDecimal quotient = realBigDecimal.divide(divisorBigDecimal, 0, roundingMode);
			final BigDecimal remainder = realBigDecimal.subtract(divisorBigDecimal.multiply(quotient));

			final RealStruct quotientReal;
			if (isQuotientFloat) {
				quotientReal = new FloatStruct(quotient);
			} else {
				final BigInteger quotientBigInteger = quotient.toBigInteger();
				quotientReal = new LongIntegerStruct(quotientBigInteger);
			}

			final BigInteger remainderBigInteger = remainder.toBigInteger();
			final LongIntegerStruct remainderInteger = new LongIntegerStruct(remainderBigInteger);

			return new QuotientRemainderResult(quotientReal, remainderInteger);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final BigIntegerStruct divisor, final RoundingMode roundingMode, final boolean isQuotientFloat) {
			return null;
		}
	}

	/**
	 * {@link RealExptVisitor} for computing exponential function results for {@link LongIntegerStruct}s.
	 */
	private static final class IntegerExptVisitor extends RealExptVisitor<LongIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerExptVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param base
		 * 		the base argument in the exponential operation
		 */
		IntegerExptVisitor(final LongIntegerStruct base) {
			super(base);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the exponential function result for {@link LongIntegerStruct}s as the {@code base} and {@code
		 * power}.
		 */
		@Override
		public NumberStruct expt(final LongIntegerStruct power) {
			if (power.minusp()) {
				return exptInteger(base, power);
			} else {
				final BigInteger baseBigInteger = base.getBigInteger();
				final BigInteger powerBigInteger = power.getBigInteger();
				final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, powerBigInteger);
				return new LongIntegerStruct(pow);
			}
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import jcl.LispStruct;
import jcl.types.IntegerType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;

/**
 * The {@link IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public class IntegerStruct extends RationalStruct {

	/**
	 * {@link IntegerStruct} constant representing 0.
	 */
	public static final IntegerStruct ZERO = new IntegerStruct(BigInteger.ZERO);

	/**
	 * {@link IntegerStruct} constant representing 1.
	 */
	public static final IntegerStruct ONE = new IntegerStruct(BigInteger.ONE);

	/**
	 * {@link IntegerStruct} constant representing 2.
	 */
	public static final IntegerStruct TWO = new IntegerStruct(BigInteger.valueOf(2));

	/**
	 * {@link IntegerStruct} constant representing -1.
	 */
	public static final IntegerStruct MINUS_ONE = new IntegerStruct(BigInteger.valueOf(-1));

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4665072618932472349L;

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
	IntegerStruct(final Apint apint) {
		this(apint.toBigInteger());
	}

	/**
	 * Public constructor.
	 *
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	IntegerStruct(final Apfloat apfloat) {
		this(apfloat.longValue());
	}

	/**
	 * Public constructor.
	 *
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public IntegerStruct(final long longValue) {
		this(BigInteger.valueOf(longValue));
	}

	/**
	 * Public constructor.
	 *
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public IntegerStruct(final BigInteger bigInteger) {
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
	public IntegerStruct(final IntegerType integerType, final BigInteger bigInteger) {
		super(integerType, null, null);
		this.bigInteger = bigInteger;
	}

	/**
	 * Getter for integer {@link #bigInteger} property.
	 *
	 * @return integer {@link #bigInteger} property
	 */
	public BigInteger getBigInteger() {
		return bigInteger;
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
	 * Determines the absolute value of this IntegerStruct.
	 */
	@Override
	public RealStruct abs() {
		if (bigInteger.signum() >= 0) {
			return this;
		}
		final BigInteger negate = bigInteger.negate();
		return new IntegerStruct(negate);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this IntegerStruct is zero using {@link #bigInteger#signum}.
	 */
	@Override
	public boolean zerop() {
		return bigInteger.signum() == 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this IntegerStruct is positive using {@link #bigInteger#signum}.
	 */
	@Override
	public boolean plusp() {
		return bigInteger.signum() > 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this IntegerStruct is negative using {@link #bigInteger#signum}.
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

	@Override
	protected NumberStruct add(final AddVisitor<?> addVisitor) {
		return addVisitor.add(this);
	}

	@Override
	protected AddVisitor<?> addVisitor() {
		return new IntegerAddVisitor(this);
	}

	@Override
	protected NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	protected SubtractVisitor<?> subtractVisitor() {
		return new IntegerSubtractVisitor(this);
	}

	@Override
	protected NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	protected MultiplyVisitor<?> multiplyVisitor() {
		return new IntegerMultiplyVisitor(this);
	}

	@Override
	protected NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	protected DivideVisitor<?> divideVisitor() {
		return new IntegerDivideVisitor(this);
	}

	@Override
	protected boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	@Override
	protected EqualToVisitor<?> equalToVisitor() {
		return new IntegerEqualToVisitor(this);
	}

	@Override
	protected boolean isLessThan(final LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	@Override
	protected LessThanVisitor<?> lessThanVisitor() {
		return new IntegerLessThanVisitor(this);
	}

	@Override
	protected boolean isGreaterThan(final GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	@Override
	protected GreaterThanVisitor<?> greaterThanVisitor() {
		return new IntegerGreaterThanVisitor(this);
	}

	@Override
	protected boolean isLessThanOrEqualTo(final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	@Override
	protected LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new IntegerLessThanOrEqualToVisitor(this);
	}

	@Override
	protected boolean isGreaterThanOrEqualTo(final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	@Override
	protected GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
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
	 * Computes the negation with {@link #bigInteger#negation} and the creating a new IntegerStruct to wrap it.
	 */
	@Override
	public NumberStruct negation() {
		final BigInteger negate = bigInteger.negate();
		return new IntegerStruct(negate);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Creates a new {@link RationalStruct} with {@link BigInteger#ONE} as the numerator and {@link #bigInteger} as the
	 * denominator.
	 */
	@Override
	public NumberStruct reciprocal() {
		return makeRational(BigInteger.ONE, bigInteger);
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
			if (power instanceof IntegerStruct) {
				return ONE;
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

	@Override
	protected ExptVisitor<?> exptVisitor() {
		return new IntegerExptVisitor(this);
	}

	/**
	 * Returns the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root.
	 *
	 * @return the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root
	 */
	public IntegerStruct isqrt() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat sqrt = ApfloatMath.sqrt(apfloat);
		final Apint floor = sqrt.floor();
		final BigInteger floorBigInteger = floor.toBigInteger();
		return new IntegerStruct(floorBigInteger);
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
		return new Apfloat(bigDecimalValue());
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
	 * Returns {@code this} as any IntegerStruct is already in rational form.
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
		return new IntegerQuotientRemainderVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@code this} as the numerator.
	 */
	@Override
	public IntegerStruct numerator() {
		return this;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #ONE} as the denominator of IntegerStructs is always '1'.
	 */
	@Override
	public IntegerStruct denominator() {
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
	public IntegerStruct gcd(final IntegerStruct integer) {
		final BigInteger gcd = bigInteger.gcd(integer.bigInteger);
		return new IntegerStruct(gcd);
	}

	/**
	 * Returns the greatest common divisor of the provided IntegerStructs. If the number of IntegerStructs provided is
	 * 0, {@link #ZERO} is returned. If the number of IntegerStructs provided is 1, that single IntegerStruct is
	 * returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used to determine the greatest common divisor
	 *
	 * @return the greatest common divisor of the provided IntegerStructs
	 */
	public static IntegerStruct gcd(final IntegerStruct... integers) {
		if (integers.length == 0) {
			return ZERO;
		}
		if (integers.length == 1) {
			return integers[0];
		}

		IntegerStruct result = integers[0];
		for (int i = 1; i < integers.length; i++) {
			final IntegerStruct currentInteger = integers[i];
			result = result.gcd(currentInteger);
		}
		return result;
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
	public IntegerStruct lcm(final IntegerStruct integer) {
		if (zerop() || integer.zerop()) {
			return ZERO;
		}

		// lcm(x y) = abs(x * y) / gcd(x y)
		final BigInteger multiply = bigInteger.multiply(integer.bigInteger);
		final BigInteger abs = multiply.abs();
		final BigInteger gcd = bigInteger.gcd(integer.bigInteger);
		final BigInteger divide = abs.divide(gcd);

		return new IntegerStruct(divide);
	}

	/**
	 * Returns the least common multiple of the provided IntegerStructs. If the number of IntegerStructs provided is 0,
	 * {@link #ONE} is returned. If the number of IntegerStructs provided is 1, that single IntegerStruct is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used to determine the least common multiple
	 *
	 * @return the least common multiple of the provided IntegerStructs
	 */
	public static IntegerStruct lcm(final IntegerStruct... integers) {
		if (integers.length == 0) {
			return ONE;
		}
		if (integers.length == 1) {
			return integers[0];
		}

		IntegerStruct result = integers[0];
		for (int i = 1; i < integers.length; i++) {
			final IntegerStruct currentInteger = integers[i];
			result = result.lcm(currentInteger);
		}
		return result;
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
	public IntegerStruct ash(final IntegerStruct count) {
		if (count.zerop()) {
			return this;
		}

		final int countInt = count.bigInteger.intValue();

		// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
		final BigInteger shiftLeft = bigInteger.shiftLeft(countInt);
		return new IntegerStruct(shiftLeft);
	}

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct logAnd(final IntegerStruct integer) {
		final BigInteger and = bigInteger.and(integer.bigInteger);
		return new IntegerStruct(and);
	}

	/**
	 * Returns the bit-wise logical 'and' of the provided IntegerStructs. If the number of IntegerStructs provided is
	 * 0, {@link #MINUS_ONE} is returned. If the number of IntegerStructs provided is 1, that single IntegerStruct is
	 * returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of the provided IntegerStructs
	 */
	public static IntegerStruct logAnd(final IntegerStruct... integers) {
		if (integers.length == 0) {
			return MINUS_ONE;
		}
		if (integers.length == 1) {
			return integers[0];
		}

		IntegerStruct result = integers[0];
		for (int i = 1; i < integers.length; i++) {
			final IntegerStruct currentInteger = integers[i];
			result = result.logAnd(currentInteger);
		}
		return result;
	}

	/**
	 * Returns the bit-wise logical 'and' of the compliment of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct logAndC1(final IntegerStruct integer) {
		final BigInteger not = bigInteger.not();
		final BigInteger and = not.and(integer.bigInteger);
		return new IntegerStruct(and);
	}

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct logAndC2(final IntegerStruct integer) {
		final BigInteger not = integer.bigInteger.not();
		final BigInteger and = bigInteger.and(not);
		return new IntegerStruct(and);
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
	public IntegerStruct logEqv(final IntegerStruct integer) {
		final BigInteger xor = bigInteger.xor(integer.bigInteger);
		final BigInteger not = xor.not();
		return new IntegerStruct(not);
	}

	/**
	 * Returns the bit-wise logical 'equivalence', or 'exclusive-nor' of the provided IntegerStructs. If the number
	 * of IntegerStructs provided is 0, {@link #MINUS_ONE} is returned. If the number of IntegerStructs provided is 1,
	 * that single IntegerStruct is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'equivalence', or 'exclusive-nor' of the provided IntegerStructs
	 */
	public static IntegerStruct logEqv(final IntegerStruct... integers) {
		if (integers.length == 0) {
			return MINUS_ONE;
		}
		if (integers.length == 1) {
			return integers[0];
		}

		IntegerStruct result = integers[0];
		for (int i = 1; i < integers.length; i++) {
			final IntegerStruct currentInteger = integers[i];
			result = result.logEqv(currentInteger);
		}
		return result;
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct logIor(final IntegerStruct integer) {
		final BigInteger or = bigInteger.or(integer.bigInteger);
		return new IntegerStruct(or);
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of the provided IntegerStructs. If the number of IntegerStructs
	 * provided is 0, {@link #ZERO} is returned. If the number of IntegerStructs provided is 1, that single
	 * IntegerStruct is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of the provided IntegerStructs
	 */
	public static IntegerStruct logIor(final IntegerStruct... integers) {
		if (integers.length == 0) {
			return ZERO;
		}
		if (integers.length == 1) {
			return integers[0];
		}

		IntegerStruct result = integers[0];
		for (int i = 1; i < integers.length; i++) {
			final IntegerStruct currentInteger = integers[i];
			result = result.logIor(currentInteger);
		}
		return result;
	}

	/**
	 * Returns the bit-wise logical compliment 'and' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical compliment 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct logNand(final IntegerStruct integer) {
		final BigInteger and = bigInteger.and(integer.bigInteger);
		final BigInteger not = and.not();
		return new IntegerStruct(not);
	}

	/**
	 * Returns the bit-wise logical compliment 'or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical compliment 'or' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct logNor(final IntegerStruct integer) {
		final BigInteger or = bigInteger.or(integer.bigInteger);
		final BigInteger not = or.not();
		return new IntegerStruct(not);
	}

	/**
	 * Returns the bit-wise logical 'not' of this IntegerStruct.
	 *
	 * @return the bit-wise logical 'not' of this IntegerStruct
	 */
	public IntegerStruct logNot() {
		final BigInteger not = bigInteger.not();
		return new IntegerStruct(not);
	}

	/**
	 * Returns the bit-wise logical 'or' of the compliment of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'or' of the compliment of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct logOrC1(final IntegerStruct integer) {
		final BigInteger not = bigInteger.not();
		final BigInteger or = not.or(integer.bigInteger);
		return new IntegerStruct(or);
	}

	/**
	 * Returns the bit-wise logical 'or' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'or' of this IntegerStruct and the compliment of provided IntegerStruct
	 */
	public IntegerStruct logOrC2(final IntegerStruct integer) {
		final BigInteger not = integer.bigInteger.not();
		final BigInteger or = bigInteger.or(not);
		return new IntegerStruct(or);
	}

	/**
	 * Returns the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct logXor(final IntegerStruct integer) {
		final BigInteger xor = bigInteger.xor(integer.bigInteger);
		return new IntegerStruct(xor);
	}

	/**
	 * Returns the bit-wise logical 'exclusive-or' of the provided IntegerStructs. If the number of IntegerStructs
	 * provided is 0, {@link #ZERO} is returned. If the number of IntegerStructs provided is 1, that single
	 * IntegerStruct is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of the provided IntegerStructs
	 */
	public static IntegerStruct logXor(final IntegerStruct... integers) {
		if (integers.length == 0) {
			return ZERO;
		}
		if (integers.length == 1) {
			return integers[0];
		}

		IntegerStruct result = integers[0];
		for (int i = 1; i < integers.length; i++) {
			final IntegerStruct currentInteger = integers[i];
			result = result.logXor(currentInteger);
		}
		return result;
	}

	/**
	 * Returns the number of bits needed to represent this IntegerStruct in binary two's-complement format.
	 *
	 * @return the number of bits needed to represent this IntegerStruct in binary two's-complement format
	 */
	public IntegerStruct integerLength() {
		final int bitLength = bigInteger.bitLength();
		final BigInteger bitLengthBigInteger = BigInteger.valueOf(bitLength);
		return new IntegerStruct(bitLengthBigInteger);
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
		final int indexInt = index.bigInteger.intValue();
		return bigInteger.testBit(indexInt);
	}

	/**
	 * Computes and returns the number of bits in the two's-complement binary representation of this IntegerStruct that
	 * are 'on' or 'set'. If this IntegerStruct is negative, the 0 bits are counted; otherwise, the 1 bits are counted.
	 *
	 * @return Computes and returns the number of bits in the two's-complement binary representation of this
	 * IntegerStruct that are 'on' or 'set'
	 */
	public IntegerStruct logCount() {
		final int bitCount = bigInteger.bitCount();
		final BigInteger bitCountBigInteger = BigInteger.valueOf(bitCount);
		return new IntegerStruct(bitCountBigInteger);
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
		final BigInteger and = bigInteger.and(integer.bigInteger);
		return and.signum() != 0;
	}

	// Visitor Implementations

	/**
	 * {@link RealAddVisitor} for computing addition results for {@link IntegerStruct}s.
	 */
	private static final class IntegerAddVisitor extends RealAddVisitor<IntegerStruct> {

		private IntegerAddVisitor(final IntegerStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for {@link IntegerStruct}s.
		 */
		@Override
		public RealStruct add(final IntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger add = bigInteger1.add(bigInteger2);
			return new IntegerStruct(add);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for an {@link IntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct add(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			final BigInteger add = multiply.add(numerator);
			return makeRational(add, denominator);
		}
	}

	/**
	 * {@link RealSubtractVisitor} for computing subtraction function results for {@link IntegerStruct}s.
	 */
	private static final class IntegerSubtractVisitor extends RealSubtractVisitor<IntegerStruct> {

		private IntegerSubtractVisitor(final IntegerStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for {@link IntegerStruct}s.
		 */
		@Override
		public RealStruct subtract(final IntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger subtract = bigInteger1.subtract(bigInteger2);
			return new IntegerStruct(subtract);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for an {@link IntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			final BigInteger subtract = multiply.subtract(numerator);
			return makeRational(subtract, denominator);
		}
	}

	/**
	 * {@link RealMultiplyVisitor} for computing multiplication function results for {@link IntegerStruct}s.
	 */
	private static final class IntegerMultiplyVisitor extends RealMultiplyVisitor<IntegerStruct> {

		private IntegerMultiplyVisitor(final IntegerStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for {@link IntegerStruct}s.
		 */
		@Override
		public RealStruct multiply(final IntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger multiply = bigInteger1.multiply(bigInteger2);
			return new IntegerStruct(multiply);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for an {@link IntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(numerator);
			return makeRational(multiply, denominator);
		}
	}

	/**
	 * {@link RealDivideVisitor} for computing division function results for {@link IntegerStruct}s.
	 */
	private static final class IntegerDivideVisitor extends RealDivideVisitor<IntegerStruct> {

		private IntegerDivideVisitor(final IntegerStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for {@link IntegerStruct}s.
		 */
		@Override
		public RealStruct divide(final IntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			return makeRational(bigInteger1, bigInteger2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for an {@link IntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			return makeRational(multiply, numerator);
		}
	}

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
	private static int getComparisonResult(final IntegerStruct number1, final IntegerStruct number2) {
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
	private static int getComparisonResult(final IntegerStruct number1, final RatioStruct number2) {
		final BigInteger bigInteger1 = number1.bigInteger;

		final BigFraction bigFraction2 = number2.getBigFraction();
		final BigFraction bigFraction2Reduced = bigFraction2.reduce();
		final BigInteger numerator = bigFraction2Reduced.getNumerator();
		final BigInteger denominator = bigFraction2Reduced.getDenominator();

		final BigInteger multiply = bigInteger1.multiply(denominator);
		return multiply.compareTo(numerator);
	}

	/**
	 * {@link RealEqualToVisitor} for computing numeric '=' equality results for {@link IntegerStruct}s.
	 */
	private static final class IntegerEqualToVisitor extends RealEqualToVisitor<IntegerStruct> {

		private IntegerEqualToVisitor(final IntegerStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '=' equality result for {@link IntegerStruct}s.
		 */
		@Override
		public boolean equalTo(final IntegerStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '=' equality result for an {@link IntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public boolean equalTo(final RatioStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}
	}

	/**
	 * {@link LessThanVisitor} for computing numeric '<' equality results for {@link IntegerStruct}s.
	 */
	private static final class IntegerLessThanVisitor extends LessThanVisitor<IntegerStruct> {

		private IntegerLessThanVisitor(final IntegerStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '<' equality result for {@link IntegerStruct}s.
		 */
		@Override
		public boolean lessThan(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '<' equality result for an {@link IntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public boolean lessThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}
	}

	/**
	 * {@link GreaterThanVisitor} for computing numeric '>' equality results for {@link IntegerStruct}s.
	 */
	private static final class IntegerGreaterThanVisitor extends GreaterThanVisitor<IntegerStruct> {

		private IntegerGreaterThanVisitor(final IntegerStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '>' equality result for {@link IntegerStruct}s.
		 */
		@Override
		public boolean greaterThan(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '>' equality result for an {@link IntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public boolean greaterThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}
	}

	/**
	 * {@link LessThanOrEqualToVisitor} for computing numeric '<=' equality results for {@link IntegerStruct}s.
	 */
	private static final class IntegerLessThanOrEqualToVisitor extends LessThanOrEqualToVisitor<IntegerStruct> {

		private IntegerLessThanOrEqualToVisitor(final IntegerStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '<=' equality result for {@link IntegerStruct}s.
		 */
		@Override
		public boolean lessThanOrEqualTo(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '<=' equality result for an {@link IntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}
	}

	/**
	 * {@link GreaterThanOrEqualToVisitor} for computing numeric '>=' equality results for {@link IntegerStruct}s.
	 */
	private static final class IntegerGreaterThanOrEqualToVisitor extends GreaterThanOrEqualToVisitor<IntegerStruct> {

		private IntegerGreaterThanOrEqualToVisitor(final IntegerStruct real1) {
			super(real1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '>=' equality result for {@link IntegerStruct}s.
		 */
		@Override
		public boolean greaterThanOrEqualTo(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '>=' equality result for an {@link IntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}
	}

	/**
	 * {@link IntegerQuotientRemainderVisitor} for computing quotient and remainder results for {@link
	 * IntegerStruct}s.
	 */
	private static final class IntegerQuotientRemainderVisitor extends RationalQuotientRemainderVisitor<IntegerStruct> {

		private IntegerQuotientRemainderVisitor(final IntegerStruct real) {
			super(real);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the quotient and remainder results for {@link IntegerStruct}s as the {@code real} and {@code
		 * divisor}.
		 */
		@Override
		public QuotientRemainderResult quotientRemainder(final IntegerStruct divisor, final RoundingMode roundingMode,
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
				quotientReal = new IntegerStruct(quotientBigInteger);
			}

			final BigInteger remainderBigInteger = remainder.toBigInteger();
			final IntegerStruct remainderInteger = new IntegerStruct(remainderBigInteger);

			return new QuotientRemainderResult(quotientReal, remainderInteger);
		}
	}

	/**
	 * {@link RealExptVisitor} for computing exponential function results for {@link IntegerStruct}s.
	 */
	private static final class IntegerExptVisitor extends RealExptVisitor<IntegerStruct> {

		private IntegerExptVisitor(final IntegerStruct base) {
			super(base);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the exponential function result for {@link IntegerStruct}s as the {@code base} and {@code power}.
		 */
		@Override
		public NumberStruct expt(final IntegerStruct power) {
			if (power.minusp()) {
				return exptInteger(base, power);
			} else {
				final BigInteger baseBigInteger = base.getBigInteger();
				final BigInteger powerBigInteger = power.getBigInteger();
				final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, powerBigInteger);
				return new IntegerStruct(pow);
			}
		}
	}

	// HashCode / Equals

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bigInteger)
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
		final IntegerStruct rhs = (IntegerStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigInteger, rhs.bigInteger)
		                          .isEquals();
	}
}

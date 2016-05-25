/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.List;

import com.google.common.math.BigIntegerMath;
import jcl.types.IntegerType;
import org.apfloat.Apint;
import org.apfloat.ApintMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link IntegerStruct2} is the object representation of a Lisp 'integer' type.
 */
public class IntegerStruct2 extends InternalNumberStruct<Apint> implements RationalStruct2 {

	/**
	 * {@link IntegerStruct2} constant representing 0.
	 */
	public static final IntegerStruct2 ZERO = valueOf(0);

	/**
	 * {@link IntegerStruct2} constant representing 1.
	 */
	public static final IntegerStruct2 ONE = valueOf(1);

	/**
	 * {@link IntegerStruct2} constant representing 2.
	 */
	public static final IntegerStruct2 TWO = valueOf(2);

	/**
	 * {@link IntegerStruct2} constant representing 10.
	 */
	public static final IntegerStruct2 TEN = valueOf(10);

	/**
	 * {@link IntegerStruct2} constant representing -1.
	 */
	public static final IntegerStruct2 MINUS_ONE = valueOf(-1);

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(IntegerStruct2.class);

	/**
	 * Private constructor.
	 *
	 * @param apint
	 * 		the value of the IntegerStruct2
	 */
	private IntegerStruct2(final Apint apint) {
		super(IntegerType.INSTANCE, apint);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@code int}. This will subclass appropriately to an {@link
	 * IntIntegerStruct}, which is the best data structure to hold an {@code int} value.
	 *
	 * @param i
	 * 		the {@link int} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link int}
	 */
	public static IntegerStruct2 valueOf(final Integer i) {
		final Apint apint = new Apint(i);
		return valueOf(apint);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@code long}. This will subclass appropriately to the
	 * IntegerStruct implementation that would most accurately hold the integer data structure.
	 *
	 * @param l
	 * 		the {@code long} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@code String}
	 */
	public static IntegerStruct2 valueOf(final Long l) {
		final Apint apint = new Apint(l);
		return valueOf(apint);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@link BigInteger}. This will subclass appropriately to
	 * the IntegerStruct implementation that would most accurately hold the integer data structure.
	 *
	 * @param bigInteger
	 * 		the {@link BigInteger} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link BigInteger}
	 */
	public static IntegerStruct2 valueOf(final BigInteger bigInteger) {
		final Apint apint = new Apint(bigInteger);
		return valueOf(apint);
	}

	/**
	 * Returns a BigIntegerStruct object with the provided {@link BigInteger} value.
	 *
	 * @param apint
	 * 		the {@link BigInteger} value of the resulting BigIntegerStruct
	 *
	 * @return a BigIntegerStruct object with the provided {@link BigInteger} value
	 */
	public static IntegerStruct2 valueOf(final Apint apint) {
		return new IntegerStruct2(apint);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@link String}. This will subclass appropriately to the
	 * IntegerStruct implementation that would most accurately hold the integer data structure.
	 *
	 * @param s
	 * 		the {@link String} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link String}
	 */
	public static IntegerStruct2 valueOf(final String s) {
		final Apint apint = new Apint(s);
		return valueOf(apint);
	}

	/**
	 * Returns this IntegerStruct as a {@code int} value.
	 *
	 * @return this IntegerStruct as a {@code int} value
	 */
	public int intValue() {
		return ap.intValue();
	}

	/**
	 * Returns this IntegerStruct as a {@code long} value.
	 *
	 * @return this IntegerStruct as a {@code long} value
	 */
	public long longValue() {
		return ap.longValue();
	}

	/**
	 * Returns this IntegerStruct as a {@link BigInteger} value.
	 *
	 * @return this IntegerStruct as a {@link BigInteger} value
	 */
	public BigInteger bigIntegerValue() {
		return ap.toBigInteger();
	}

	/**
	 * Returns the greatest common divisor of the provided IntegerStructs. If the number of IntegerStructs provided is
	 * 0, {@link #ZERO} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used to determine the greatest common divisor
	 *
	 * @return the greatest common divisor of the provided IntegerStructs
	 */
	public static IntegerStruct2 gcd(final List<IntegerStruct2> integers) {
		return integers.stream().reduce(ZERO, IntegerStruct2::gcd);
	}

	/**
	 * Returns the greatest common divisor between this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct in comparison to this IntegerStruct to determine the greatest common divisor
	 *
	 * @return the greatest common divisor between this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct2 gcd(final IntegerStruct2 integer) {
		final Apint apint1 = integer.ap;
		final Apint gcd = ApintMath.gcd(ap, apint1);
		return valueOf(gcd);
	}

	/**
	 * Returns the least common multiple of the provided IntegerStructs. If the number of IntegerStructs provided is 0,
	 * {@link #ONE} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used to determine the least common multiple
	 *
	 * @return the least common multiple of the provided IntegerStructs
	 */
	static IntegerStruct2 lcm(final List<IntegerStruct2> integers) {
		return integers.stream().reduce(ONE, IntegerStruct2::lcm);
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
	public IntegerStruct2 lcm(final IntegerStruct2 integer) {
		final Apint apint1 = integer.ap;
		final Apint lcm = ApintMath.lcm(ap, apint1);
		return valueOf(lcm);
	}

	/**
	 * Performs the arithmetic shift operation on the binary representation of this IntegerStruct, shifting the bits
	 * left or right by the provided {@code count} IntegerStruct based on its sign. If the {@code count} value is '0',
	 * the result is {@code this}.
	 *
	 * @param count
	 * 		the bit positions to shift this IntegerStruct left or right
	 *
	 * @return the arithmetic shift operation on the binary representation of this IntegerStruct
	 */
	public IntegerStruct2 ash(final IntegerStruct2 count) {
		if (count.zerop()) {
			return this;
		}
		final BigInteger countBigInteger = count.ap.toBigInteger();

		int countI;
		try {
			countI = countBigInteger.intValueExact();
		} catch (final ArithmeticException ignore) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Forcibly migrated {} to an int for bit-shifting.", countBigInteger);
			}
			countI = countBigInteger.intValue();
		}

		// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
		final BigInteger shiftedBigInteger = ap.toBigInteger().shiftLeft(countI);
		return valueOf(shiftedBigInteger);
	}

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct2 logAnd(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		return valueOf(bigInteger1.and(bigInteger2));
	}

	/**
	 * Returns the bit-wise logical 'and' of the provided IntegerStructs. If the number of IntegerStructs provided is
	 * 0, {@link #MINUS_ONE} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of the provided IntegerStructs
	 */
	public static IntegerStruct2 logAnd(final List<IntegerStruct2> integers) {
		return integers.stream().reduce(MINUS_ONE, IntegerStruct2::logAnd);
	}

	/**
	 * Returns the bit-wise logical 'and' of the compliment of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct2 logAndC1(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		return valueOf(bigInteger1.not().and(bigInteger2));
	}

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct2 logAndC2(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		return valueOf(bigInteger1.and(bigInteger2.not()));
	}

	/**
	 * Returns the bit-wise logical 'equivalence', or 'exclusive-nor' of the provided IntegerStructs. If the number
	 * of IntegerStructs provided is 0, {@link #MINUS_ONE} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'equivalence', or 'exclusive-nor' of the provided IntegerStructs
	 */
	public static IntegerStruct2 logEqv(final List<IntegerStruct2> integers) {
		return integers.stream().reduce(MINUS_ONE, IntegerStruct2::logEqv);
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
	public IntegerStruct2 logEqv(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		final BigInteger xor = bigInteger1.xor(bigInteger2);
		return valueOf(xor.not());
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of the provided IntegerStructs. If the number of IntegerStructs
	 * provided is 0, {@link #ZERO} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of the provided IntegerStructs
	 */
	public static IntegerStruct2 logIor(final List<IntegerStruct2> integers) {
		return integers.stream().reduce(ZERO, IntegerStruct2::logIor);
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct2 logIor(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		return valueOf(bigInteger1.or(bigInteger2));
	}

	/**
	 * Returns the bit-wise logical 'nand' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'nand' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct2 logNand(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		final BigInteger and = bigInteger1.and(bigInteger2);
		return valueOf(and.not());
	}

	/**
	 * Returns the bit-wise logical 'nor' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'nor' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct2 logNor(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		final BigInteger or = bigInteger1.or(bigInteger2);
		return valueOf(or.not());
	}

	/**
	 * Returns the bit-wise logical 'not' of this IntegerStruct.
	 *
	 * @return the bit-wise logical 'not' of this IntegerStruct
	 */
	public IntegerStruct2 logNot() {
		final BigInteger bigInteger = ap.toBigInteger();
		return valueOf(bigInteger.not());
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of the compliment of this IntegerStruct and the provided
	 * IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of the compliment of this IntegerStruct and the provided
	 * IntegerStruct
	 */
	public IntegerStruct2 logOrC1(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		return valueOf(bigInteger1.not().or(bigInteger2));
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct and the compliment of provided IntegerStruct
	 */
	public IntegerStruct2 logOrC2(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		return valueOf(bigInteger1.or(bigInteger2.not()));
	}

	/**
	 * Returns the bit-wise logical 'exclusive-or' of the provided IntegerStructs. If the number of IntegerStructs
	 * provided is 0, {@link #ZERO} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of the provided IntegerStructs
	 */
	public static IntegerStruct2 logXor(final List<IntegerStruct2> integers) {
		return integers.stream().reduce(ZERO, IntegerStruct2::logXor);
	}

	/**
	 * Returns the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStruct2 logXor(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		return valueOf(bigInteger1.xor(bigInteger2));
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
	public boolean logBitP(final IntegerStruct2 index) {
		final BigInteger bigInteger = ap.toBigInteger();
		final int indexInt = index.intValue();
		return bigInteger.testBit(indexInt);
	}

	/**
	 * Computes and returns the number of bits in the two's-complement binary representation of this IntegerStruct that
	 * are 'on' or 'set'. If this IntegerStruct is negative, the 0 bits are counted; otherwise, the 1 bits are counted.
	 *
	 * @return Computes and returns the number of bits in the two's-complement binary representation of this
	 * IntegerStruct that are 'on' or 'set'
	 */
	public IntegerStruct2 logCount() {
		final BigInteger bigInteger = ap.toBigInteger();
		final int bitCount = bigInteger.bitCount();
		return valueOf(bitCount);
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
	public boolean logTest(final IntegerStruct2 integer) {
		final IntegerStruct2 and = logAnd(integer);
		return !and.zerop();
	}

	/**
	 * Returns the number of bits needed to represent this IntegerStruct in binary two's-complement format.
	 *
	 * @return the number of bits needed to represent this IntegerStruct in binary two's-complement format
	 */
	public IntegerStruct2 integerLength() {
		final BigInteger bigInteger = ap.toBigInteger();
		final int bitLength = bigInteger.bitLength();
		return valueOf(bitLength);
	}

	/**
	 * Returns true if this IntegerStruct is even (divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct is even (divisible by two); otherwise, false
	 */
	public boolean evenp() {
		final BigInteger bigInteger = ap.toBigInteger();
		return !bigInteger.testBit(0);
	}

	/**
	 * Returns true if this IntegerStruct is odd (not divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct is odd (not divisible by two); otherwise, false
	 */
	public boolean oddp() {
		final BigInteger bigInteger = ap.toBigInteger();
		return bigInteger.testBit(0);
	}

	/**
	 * Returns the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root.
	 *
	 * @return the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root
	 */
	public IntegerStruct2 isqrt() {
		final BigInteger bigInteger = ap.toBigInteger();
		final BigInteger sqrtFloor = BigIntegerMath.sqrt(bigInteger, RoundingMode.FLOOR);
		return valueOf(sqrtFloor);
	}

	/*
		RationalStruct
	 */

	@Override
	public IntegerStruct2 numerator() {
		return this;
	}

	@Override
	public IntegerStruct2 denominator() {
		return ONE;
	}

	@Override
	public boolean isLessThan(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isGreaterThan(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean plusp() {
		return false;
	}

	@Override
	public boolean minusp() {
		return false;
	}

	@Override
	public FloatStruct2 floatingPoint() {
		return null;
	}

	@Override
	public FloatStruct2 floatingPoint(final FloatStruct2 prototype) {
		return null;
	}

	@Override
	public RealStruct2 mod(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public RealStruct2 rem(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public ComplexStruct cis() {
		return null;
	}

	@Override
	public QuotientRemainderResult floor(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult ffloor(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult ceiling(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult fceiling(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult truncate(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult ftruncate(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult round(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult fround(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public RealStruct2 abs() {
		return null;
	}

	@Override
	public boolean zerop() {
		return false;
	}

	@Override
	public NumberStruct2 add(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 subtract(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 multiply(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 divide(final NumberStruct2 number) {
		return null;
	}

	@Override
	public boolean isEqualTo(final NumberStruct2 number) {
		return false;
	}

	@Override
	public NumberStruct2 negation() {
		return null;
	}

	@Override
	public NumberStruct2 reciprocal() {
		return null;
	}

	@Override
	public NumberStruct2 exp() {
		return null;
	}

	@Override
	public NumberStruct2 expt(final NumberStruct2 power) {
		return null;
	}

	@Override
	public NumberStruct2 log() {
		return null;
	}

	@Override
	public NumberStruct2 sqrt() {
		return null;
	}

	@Override
	public NumberStruct2 sin() {
		return null;
	}

	@Override
	public NumberStruct2 cos() {
		return null;
	}

	@Override
	public NumberStruct2 tan() {
		return null;
	}

	@Override
	public NumberStruct2 asin() {
		return null;
	}

	@Override
	public NumberStruct2 acos() {
		return null;
	}

	@Override
	public NumberStruct2 atan() {
		return null;
	}

	@Override
	public NumberStruct2 sinh() {
		return null;
	}

	@Override
	public NumberStruct2 cosh() {
		return null;
	}

	@Override
	public NumberStruct2 tanh() {
		return null;
	}

	@Override
	public NumberStruct2 asinh() {
		return null;
	}

	@Override
	public NumberStruct2 acosh() {
		return null;
	}

	@Override
	public NumberStruct2 atanh() {
		return null;
	}
}

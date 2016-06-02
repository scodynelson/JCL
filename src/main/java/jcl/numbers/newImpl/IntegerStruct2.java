/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import java.math.BigInteger;
import java.util.List;

import jcl.types.IntegerType;
import org.apfloat.Apcomplex;
import org.apfloat.Apint;
import org.apfloat.ApintMath;
import org.apfloat.Aprational;

/**
 * The {@link IntegerStruct2} is the object representation of a Lisp 'integer' type.
 */
public final class IntegerStruct2 extends RationalStruct2Impl<Apint> {

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
	 * Private constructor.
	 *
	 * @param apint
	 * 		the value of the IntegerStruct2
	 */
	private IntegerStruct2(final Apint apint) {
		super(IntegerType.INSTANCE, apint);
	}

	/**
	 * Returns a new IntegerStruct2 representing the provided {@link Integer}.
	 *
	 * @param i
	 * 		the {@link Integer} representing the new IntegerStruct2
	 *
	 * @return a new IntegerStruct2 representing the provided {@link Integer}
	 */
	public static IntegerStruct2 valueOf(final Integer i) {
		final Apint apint = new Apint(i);
		return valueOf(apint);
	}

	/**
	 * Returns a new IntegerStruct2 representing the provided {@link Long}.
	 *
	 * @param l
	 * 		the {@link Long} representing the new IntegerStruct2
	 *
	 * @return a new IntegerStruct2 representing the provided {@link Long}
	 */
	public static IntegerStruct2 valueOf(final Long l) {
		final Apint apint = new Apint(l);
		return valueOf(apint);
	}

	/**
	 * Returns a new IntegerStruct2 representing the provided {@link BigInteger}.
	 *
	 * @param bigInteger
	 * 		the {@link BigInteger} representing the new IntegerStruct2
	 *
	 * @return a new IntegerStruct2 representing the provided {@link BigInteger}
	 */
	public static IntegerStruct2 valueOf(final BigInteger bigInteger) {
		final Apint apint = new Apint(bigInteger);
		return valueOf(apint);
	}

	/**
	 * Returns a new IntegerStruct2 representing the provided {@link String}.
	 *
	 * @param s
	 * 		the {@link String} representing the new IntegerStruct2
	 *
	 * @return a new IntegerStruct2 representing the provided {@link String}
	 */
	public static IntegerStruct2 valueOf(final String s) {
		final Apint apint = new Apint(s);
		return valueOf(apint);
	}

	/**
	 * Returns a IntegerStruct2 object with the provided {@link Apint} value.
	 *
	 * @param apint
	 * 		the {@link Apint} value of the resulting IntegerStruct2
	 *
	 * @return a IntegerStruct2 object with the provided {@link Apint} value
	 */
	public static IntegerStruct2 valueOf(final Apint apint) {
		return new IntegerStruct2(apint);
	}

	/**
	 * Returns this IntegerStruct2 as a {@code int} value.
	 *
	 * @return this IntegerStruct2 as a {@code int} value
	 */
	public int intValue() {
		return ap.intValue();
	}

	/**
	 * Returns this IntegerStruct2 as a {@code long} value.
	 *
	 * @return this IntegerStruct2 as a {@code long} value
	 */
	public long longValue() {
		return ap.longValue();
	}

	/**
	 * Returns this IntegerStruct2 as a {@link BigInteger} value.
	 *
	 * @return this IntegerStruct2 as a {@link BigInteger} value
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
	 * Returns the greatest common divisor between this IntegerStruct2 and the provided IntegerStruct2.
	 *
	 * @param integer
	 * 		the IntegerStruct2 in comparison to this IntegerStruct2 to determine the greatest common divisor
	 *
	 * @return the greatest common divisor between this IntegerStruct2 and the provided IntegerStruct2
	 */
	public IntegerStruct2 gcd(final IntegerStruct2 integer) {
		final Apint gcd = ApintMath.gcd(ap, integer.ap);
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
	public static IntegerStruct2 lcm(final List<IntegerStruct2> integers) {
		return integers.stream().reduce(ONE, IntegerStruct2::lcm);
	}

	/**
	 * Returns the least common multiple between this IntegerStruct2 and the provided IntegerStruct2. If this or the
	 * provided IntegerStruct2 are '0', the result is {@link #ZERO}.
	 *
	 * @param integer
	 * 		the IntegerStruct2 in comparison to this IntegerStruct2 to determine the least common multiple
	 *
	 * @return the least common multiple between this IntegerStruct2 and the provided IntegerStruct2
	 */
	public IntegerStruct2 lcm(final IntegerStruct2 integer) {
		final Apint lcm = ApintMath.lcm(ap, integer.ap);
		return valueOf(lcm);
	}

	/**
	 * Performs the arithmetic shift operation on the binary representation of this IntegerStruct2, shifting the bits
	 * left or right by the provided {@code count} IntegerStruct2 based on its sign. If the {@code count} value is '0',
	 * the result is {@code this}.
	 *
	 * @param count
	 * 		the bit positions to shift this IntegerStruct2 left or right
	 *
	 * @return the arithmetic shift operation on the binary representation of this IntegerStruct2
	 */
	public IntegerStruct2 ash(final IntegerStruct2 count) {
		final Apint countAp = count.ap;
		if (countAp.signum() == 0) {
			return this;
		}
		final int countI = countAp.intValue();

		// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
		final BigInteger shiftedBigInteger = ap.toBigInteger().shiftLeft(countI);
		return valueOf(shiftedBigInteger);
	}

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct2 and the provided IntegerStruct2.
	 *
	 * @param integer
	 * 		the IntegerStruct2 used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct2 and the provided IntegerStruct2
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
	 * Returns the bit-wise logical 'and' of the compliment of this IntegerStruct2 and the provided IntegerStruct2.
	 *
	 * @param integer
	 * 		the IntegerStruct2 used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct2 and the provided IntegerStruct2
	 */
	public IntegerStruct2 logAndC1(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		return valueOf(bigInteger1.not().and(bigInteger2));
	}

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct2 and the compliment of provided IntegerStruct2.
	 *
	 * @param integer
	 * 		the IntegerStruct2 used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct2 and the provided IntegerStruct2
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
	 * Returns the bit-wise logical 'equivalence', or 'exclusive-nor' of this IntegerStruct2 and the provided
	 * IntegerStruct2.
	 *
	 * @param integer
	 * 		the IntegerStruct2 used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'equivalence', or 'exclusive-nor' of this IntegerStruct2 and the provided
	 * IntegerStruct2
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
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct2 and the provided IntegerStruct2.
	 *
	 * @param integer
	 * 		the IntegerStruct2 used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct2 and the provided IntegerStruct2
	 */
	public IntegerStruct2 logIor(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		return valueOf(bigInteger1.or(bigInteger2));
	}

	/**
	 * Returns the bit-wise logical 'nand' of this IntegerStruct2 and the provided IntegerStruct2.
	 *
	 * @param integer
	 * 		the IntegerStruct2 used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'nand' of this IntegerStruct2 and the provided IntegerStruct2
	 */
	public IntegerStruct2 logNand(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		final BigInteger and = bigInteger1.and(bigInteger2);
		return valueOf(and.not());
	}

	/**
	 * Returns the bit-wise logical 'nor' of this IntegerStruct2 and the provided IntegerStruct2.
	 *
	 * @param integer
	 * 		the IntegerStruct2 used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'nor' of this IntegerStruct2 and the provided IntegerStruct2
	 */
	public IntegerStruct2 logNor(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		final BigInteger or = bigInteger1.or(bigInteger2);
		return valueOf(or.not());
	}

	/**
	 * Returns the bit-wise logical 'not' of this IntegerStruct2.
	 *
	 * @return the bit-wise logical 'not' of this IntegerStruct2
	 */
	public IntegerStruct2 logNot() {
		final BigInteger bigInteger = ap.toBigInteger();
		return valueOf(bigInteger.not());
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of the compliment of this IntegerStruct2 and the provided
	 * IntegerStruct2.
	 *
	 * @param integer
	 * 		the IntegerStruct2 used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of the compliment of this IntegerStruct2 and the provided
	 * IntegerStruct2
	 */
	public IntegerStruct2 logOrC1(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		return valueOf(bigInteger1.not().or(bigInteger2));
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct2 and the compliment of provided IntegerStruct2.
	 *
	 * @param integer
	 * 		the IntegerStruct2 used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct2 and the compliment of provided IntegerStruct2
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
	 * Returns the bit-wise logical 'exclusive-or' of this IntegerStruct2 and the provided IntegerStruct2.
	 *
	 * @param integer
	 * 		the IntegerStruct2 used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of this IntegerStruct2 and the provided IntegerStruct2
	 */
	public IntegerStruct2 logXor(final IntegerStruct2 integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap.toBigInteger();
		return valueOf(bigInteger1.xor(bigInteger2));
	}

	/**
	 * Returns true if the bit in this IntegerStruct2 whose index is {@code index} is a one-bit; otherwise, returns
	 * false.
	 *
	 * @param index
	 * 		the index value to test this IntegerStruct2 for a one-bit
	 *
	 * @return true if the bit in this IntegerStruct2 whose index is {@code index} is a one-bit; otherwise, false
	 */
	public boolean logBitP(final IntegerStruct2 index) {
		final BigInteger bigInteger = ap.toBigInteger();
		final int indexInt = index.intValue();
		return bigInteger.testBit(indexInt);
	}

	/**
	 * Computes and returns the number of bits in the two's-complement binary representation of this IntegerStruct2 that
	 * are 'on' or 'set'. If this IntegerStruct2 is negative, the 0 bits are counted; otherwise, the 1 bits are counted.
	 *
	 * @return Computes and returns the number of bits in the two's-complement binary representation of this
	 * IntegerStruct2 that are 'on' or 'set'
	 */
	public IntegerStruct2 logCount() {
		final BigInteger bigInteger = ap.toBigInteger();
		final int bitCount = bigInteger.bitCount();
		return valueOf(bitCount);
	}

	/**
	 * Returns true if any of the bits designated by the 1's in this IntegerStruct2 are 1 in the provided
	 * IntegerStruct2; otherwise, returns false.
	 *
	 * @param integer
	 * 		the IntegerStruct2 used in the test comparison to this IntegerStruct2
	 *
	 * @return true if any of the bits designated by the 1's in this IntegerStruct2 are 1 in the provided
	 * IntegerStruct2; otherwise, false.
	 */
	public boolean logTest(final IntegerStruct2 integer) {
		final IntegerStruct2 and = logAnd(integer);
		return and.ap.signum() != 0;
	}

	/**
	 * Returns the number of bits needed to represent this IntegerStruct2 in binary two's-complement format.
	 *
	 * @return the number of bits needed to represent this IntegerStruct2 in binary two's-complement format
	 */
	public IntegerStruct2 integerLength() {
		final BigInteger bigInteger = ap.toBigInteger();
		final int bitLength = bigInteger.bitLength();
		return valueOf(bitLength);
	}

	/**
	 * Returns true if this IntegerStruct2 is even (divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct2 is even (divisible by two); otherwise, false
	 */
	public boolean evenp() {
		// TODO: Apint(2) constant??
		// TODO: performance over biginteger
		return ap.mod(new Apint(2)).equals(Apcomplex.ZERO);
//		final BigInteger bigInteger = ap.toBigInteger();
//		return !bigInteger.testBit(0);
	}

	/**
	 * Returns true if this IntegerStruct2 is odd (not divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct2 is odd (not divisible by two); otherwise, false
	 */
	public boolean oddp() {
		return !evenp();
		// TODO: performance over biginteger
//		final BigInteger bigInteger = ap.toBigInteger();
//		return bigInteger.testBit(0);
	}

	/**
	 * Returns the greatest IntegerStruct2 less than or equal to this IntegerStructs exact positive square root.
	 *
	 * @return the greatest IntegerStruct2 less than or equal to this IntegerStructs exact positive square root
	 */
	public IntegerStruct2 isqrt() {
		final Apint[] sqrt = ApintMath.sqrt(ap);
		return valueOf(sqrt[0]);
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

	/*
		RealStruct
	 */

	@Override
	public IntegerStruct2 rational() {
		return this;
	}

	@Override
	public RealStruct2 mod(final RealStruct2 divisor) {
		// TODO
		final QuotientRemainderResult2 floor = floor(divisor);
		return floor.getRemainder();
	}

	@Override
	public RealStruct2 rem(final RealStruct2 divisor) {
		// TODO
		final QuotientRemainderResult2 truncate = truncate(divisor);
		return truncate.getRemainder();
	}

	/*
		NumberStruct
	 */

	@Override
	public Apint ap() {
		return ap;
	}

	@Override
	public IntegerStruct2 abs() {
		final Apint abs = ApintMath.abs(ap);
		return valueOf(abs);
	}

	@Override
	public NumberStruct2 add(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apint) {
			final Apint add = ap.add((Apint) numberAp);
			return valueOf(add);
		}
		return super.add(number);
	}

	@Override
	public NumberStruct2 subtract(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apint) {
			final Apint subtract = ap.subtract((Apint) numberAp);
			return valueOf(subtract);
		}
		return super.subtract(number);
	}

	@Override
	public NumberStruct2 multiply(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apint) {
			final Apint multiply = ap.multiply((Apint) numberAp);
			return valueOf(multiply);
		}
		return super.multiply(number);
	}

	@Override
	public NumberStruct2 divide(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apint) {
			final Apint divide = ap.divide((Apint) numberAp);
			return valueOf(divide);
		}
		return super.divide(number);
	}

	@Override
	public boolean isEqualTo(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apint) {
			final Apint integerAp = (Apint) numberAp;
			final boolean shouldReverseCompare = ap.preferCompare(integerAp);
			return shouldReverseCompare ? integerAp.equals(ap) : ap.equals(integerAp);
		}
		return super.isEqualTo(number);
	}

	@Override
	public NumberStruct2 signum() {
		// TODO
		return super.signum();
	}

	@Override
	public IntegerStruct2 realPart() {
		return this;
	}

	@Override
	public IntegerStruct2 conjugate() {
		return this;
	}

	@Override
	public IntegerStruct2 negation() {
		final Apint negate = ap.negate();
		return valueOf(negate);
	}

	@Override
	public RationalStruct2 reciprocal() {
		final Aprational reciprocal = new Aprational(Apcomplex.ONE, ap);
		return RationalStruct2.valueOf(reciprocal);
	}

	/*
		ToString
	 */

	@Override
	public String toString() {
		return ap.toString();
	}
}

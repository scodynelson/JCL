/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigInteger;
import java.util.List;

/**
 * The {@link IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public interface IntegerStruct extends RationalStruct {

	/**
	 * {@link IntegerStruct} constant representing 0.
	 */
	IntegerStruct ZERO = IntIntegerStruct.ZERO;

	/**
	 * {@link IntegerStruct} constant representing 1.
	 */
	IntegerStruct ONE = IntIntegerStruct.ONE;

	/**
	 * {@link IntegerStruct} constant representing 2.
	 */
	IntegerStruct TWO = IntIntegerStruct.TWO;

	/**
	 * {@link IntegerStruct} constant representing 10.
	 */
	IntegerStruct TEN = IntIntegerStruct.TEN;

	/**
	 * {@link IntegerStruct} constant representing -1.
	 */
	IntegerStruct MINUS_ONE = IntIntegerStruct.MINUS_ONE;

	static IntegerStruct valueOf(final int i) {
		return IntIntegerStruct.valueOf(i);
	}

	static IntegerStruct valueOf(final long l) {
		return LongIntegerStruct.valueOf(l);
	}

	static IntegerStruct valueOf(final BigInteger bigInteger) {
		return BigIntegerStruct.valueOf(bigInteger);
	}

	static IntegerStruct valueOf(final String s) {
		try {
			final int i = Integer.parseInt(s);
			return valueOf(i);
		} catch (final NumberFormatException ignore) {
		}
		try {
			final long l = Long.parseLong(s);
			return valueOf(l);
		} catch (final NumberFormatException ignore) {
		}
		final BigInteger bigInteger = new BigInteger(s);
		return valueOf(bigInteger);
	}

	BigInteger getBigInteger();

	/**
	 * Returns true if this IntegerStruct is even (divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct is even (divisible by two); otherwise, false
	 */
	boolean evenp();

	/**
	 * Returns true if this IntegerStruct is odd (not divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct is odd (not divisible by two); otherwise, false
	 */
	boolean oddp();

	/**
	 * Returns the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root.
	 *
	 * @return the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root
	 */
	IntegerStruct isqrt();


	/**
	 * Returns the greatest common divisor between this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct in comparison to this IntegerStruct to determine the greatest common divisor
	 *
	 * @return the greatest common divisor between this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct gcd(final IntegerStruct integer);

	/**
	 * Returns the greatest common divisor of the provided IntegerStructs. If the number of IntegerStructs provided is
	 * 0, {@link #ZERO} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used to determine the greatest common divisor
	 *
	 * @return the greatest common divisor of the provided IntegerStructs
	 */
	static IntegerStruct gcd(final List<IntegerStruct> integers) {
		return integers.stream().reduce(ZERO, IntegerStruct::gcd);
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
	IntegerStruct lcm(final IntegerStruct integer);

	/**
	 * Returns the least common multiple of the provided IntegerStructs. If the number of IntegerStructs provided is 0,
	 * {@link #ONE} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used to determine the least common multiple
	 *
	 * @return the least common multiple of the provided IntegerStructs
	 */
	static IntegerStruct lcm(final List<IntegerStruct> integers) {
		return integers.stream().reduce(ONE, IntegerStruct::lcm);
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
	IntegerStruct ash(final IntegerStruct count);

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logAnd(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'and' of the provided IntegerStructs. If the number of IntegerStructs provided is
	 * 0, {@link #MINUS_ONE} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of the provided IntegerStructs
	 */
	static IntegerStruct logAnd(final List<IntegerStruct> integers) {
		return integers.stream().reduce(MINUS_ONE, IntegerStruct::logAnd);
	}

	/**
	 * Returns the bit-wise logical 'and' of the compliment of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logAndC1(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logAndC2(final IntegerStruct integer);

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
	IntegerStruct logEqv(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'equivalence', or 'exclusive-nor' of the provided IntegerStructs. If the number
	 * of IntegerStructs provided is 0, {@link #MINUS_ONE} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'equivalence', or 'exclusive-nor' of the provided IntegerStructs
	 */
	static IntegerStruct logEqv(final List<IntegerStruct> integers) {
		return integers.stream().reduce(MINUS_ONE, IntegerStruct::logEqv);
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logIor(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'inclusive-or' of the provided IntegerStructs. If the number of IntegerStructs
	 * provided is 0, {@link #ZERO} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of the provided IntegerStructs
	 */
	static IntegerStruct logIor(final List<IntegerStruct> integers) {
		return integers.stream().reduce(ZERO, IntegerStruct::logIor);
	}

	/**
	 * Returns the bit-wise logical compliment 'and' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical compliment 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logNand(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical compliment 'or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical compliment 'or' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logNor(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'not' of this IntegerStruct.
	 *
	 * @return the bit-wise logical 'not' of this IntegerStruct
	 */
	IntegerStruct logNot();

	/**
	 * Returns the bit-wise logical 'or' of the compliment of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'or' of the compliment of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logOrC1(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'or' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'or' of this IntegerStruct and the compliment of provided IntegerStruct
	 */
	IntegerStruct logOrC2(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logXor(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'exclusive-or' of the provided IntegerStructs. If the number of IntegerStructs
	 * provided is 0, {@link #ZERO} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of the provided IntegerStructs
	 */
	static IntegerStruct logXor(final List<IntegerStruct> integers) {
		return integers.stream().reduce(ZERO, IntegerStruct::logXor);
	}

	/**
	 * Returns the number of bits needed to represent this IntegerStruct in binary two's-complement format.
	 *
	 * @return the number of bits needed to represent this IntegerStruct in binary two's-complement format
	 */
	IntegerStruct integerLength();

	/**
	 * Returns true if the bit in this IntegerStruct whose index is {@code index} is a one-bit; otherwise, returns
	 * false.
	 *
	 * @param index
	 * 		the index value to test this IntegerStruct for a one-bit
	 *
	 * @return true if the bit in this IntegerStruct whose index is {@code index} is a one-bit; otherwise, false
	 */
	boolean logBitP(final IntegerStruct index);

	/**
	 * Computes and returns the number of bits in the two's-complement binary representation of this IntegerStruct that
	 * are 'on' or 'set'. If this IntegerStruct is negative, the 0 bits are counted; otherwise, the 1 bits are counted.
	 *
	 * @return Computes and returns the number of bits in the two's-complement binary representation of this
	 * IntegerStruct that are 'on' or 'set'
	 */
	IntegerStruct logCount();

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
	boolean logTest(final IntegerStruct integer);
}

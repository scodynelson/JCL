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
		if ((l <= Integer.MAX_VALUE) || (l >= Integer.MIN_VALUE)) {
			return IntIntegerStruct.valueOf(Long.valueOf(l).intValue());
		}
		return LongIntegerStruct.valueOf(l);
	}

	static IntegerStruct valueOf(final BigInteger bigInteger) {
		try {
			final int i = bigInteger.intValueExact();
			return valueOf(i);
		} catch (final ArithmeticException ignore) {
		}
		try {
			final long l = bigInteger.longValueExact();
			return valueOf(l);
		} catch (final ArithmeticException ignore) {
		}
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

	@Deprecated
	BigInteger getBigInteger();

	int intValue();

	long longValue();

	BigInteger bigIntegerValue();

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
	default IntegerStruct gcd(final IntegerStruct integer) {
		final GcdVisitor<?> gcdVisitor = gcdVisitor();
		return integer.gcd(gcdVisitor);
	}

	IntegerStruct gcd(GcdVisitor<?> gcdVisitor);

	GcdVisitor<?> gcdVisitor();

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
	default IntegerStruct lcm(final IntegerStruct integer) {
		final LcmVisitor<?> lcmVisitor = lcmVisitor();
		return integer.lcm(lcmVisitor);
	}

	IntegerStruct lcm(LcmVisitor<?> lcmVisitor);

	LcmVisitor<?> lcmVisitor();

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
	default IntegerStruct ash(final IntegerStruct count) {
		final AshVisitor<?> ashVisitor = ashVisitor();
		return count.ash(ashVisitor);
	}

	IntegerStruct ash(AshVisitor<?> ashVisitor);

	AshVisitor<?> ashVisitor();

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	default IntegerStruct logAnd(final IntegerStruct integer) {
		final LogAndVisitor<?> logAndVisitor = logAndVisitor();
		return integer.logAnd(logAndVisitor);
	}

	IntegerStruct logAnd(LogAndVisitor<?> logAndVisitor);

	LogAndVisitor<?> logAndVisitor();

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
	default IntegerStruct logAndC1(final IntegerStruct integer) {
		final LogAndC1Visitor<?> logAndC1Visitor = logAndC1Visitor();
		return integer.logAndC1(logAndC1Visitor);
	}

	IntegerStruct logAndC1(LogAndC1Visitor<?> logAndC1Visitor);

	LogAndC1Visitor<?> logAndC1Visitor();

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	default IntegerStruct logAndC2(final IntegerStruct integer) {
		final LogAndC2Visitor<?> logAndC2Visitor = logAndC2Visitor();
		return integer.logAndC2(logAndC2Visitor);
	}

	IntegerStruct logAndC2(LogAndC2Visitor<?> logAndC2Visitor);

	LogAndC2Visitor<?> logAndC2Visitor();

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
	default IntegerStruct logEqv(final IntegerStruct integer) {
		final LogEqvVisitor<?> logEqvVisitor = logEqvVisitor();
		return integer.logEqv(logEqvVisitor);
	}

	IntegerStruct logEqv(LogEqvVisitor<?> logEqvVisitor);

	LogEqvVisitor<?> logEqvVisitor();

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
	default IntegerStruct logIor(final IntegerStruct integer) {
		final LogIorVisitor<?> logIorVisitor = logIorVisitor();
		return integer.logIor(logIorVisitor);
	}

	IntegerStruct logIor(LogIorVisitor<?> logIorVisitor);

	LogIorVisitor<?> logIorVisitor();

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
	default IntegerStruct logNand(final IntegerStruct integer) {
		final LogNandVisitor<?> logNandVisitor = logNandVisitor();
		return integer.logNand(logNandVisitor);
	}

	IntegerStruct logNand(LogNandVisitor<?> logNandVisitor);

	LogNandVisitor<?> logNandVisitor();

	/**
	 * Returns the bit-wise logical compliment 'or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical compliment 'or' of this IntegerStruct and the provided IntegerStruct
	 */
	default IntegerStruct logNor(final IntegerStruct integer) {
		final LogNorVisitor<?> logNorVisitor = logNorVisitor();
		return integer.logNor(logNorVisitor);
	}

	IntegerStruct logNor(LogNorVisitor<?> logNorVisitor);

	LogNorVisitor<?> logNorVisitor();

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
	default IntegerStruct logOrC1(final IntegerStruct integer) {
		final LogOrC1Visitor<?> logOrC1Visitor = logOrC1Visitor();
		return integer.logOrC1(logOrC1Visitor);
	}

	IntegerStruct logOrC1(LogOrC1Visitor<?> logOrC1Visitor);

	LogOrC1Visitor<?> logOrC1Visitor();

	/**
	 * Returns the bit-wise logical 'or' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'or' of this IntegerStruct and the compliment of provided IntegerStruct
	 */
	default IntegerStruct logOrC2(final IntegerStruct integer) {
		final LogOrC2Visitor<?> logOrC2Visitor = logOrC2Visitor();
		return integer.logOrC2(logOrC2Visitor);
	}

	IntegerStruct logOrC2(LogOrC2Visitor<?> logOrC2Visitor);

	LogOrC2Visitor<?> logOrC2Visitor();

	/**
	 * Returns the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	default IntegerStruct logXor(final IntegerStruct integer) {
		final LogXorVisitor<?> logXorVisitor = logXorVisitor();
		return integer.logXor(logXorVisitor);
	}

	IntegerStruct logXor(LogXorVisitor<?> logXorVisitor);

	LogXorVisitor<?> logXorVisitor();

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
	default boolean logBitP(final IntegerStruct index) {
		final LogBitPVisitor<?> logBitPVisitor = logBitPVisitor();
		return index.logBitP(logBitPVisitor);
	}

	boolean logBitP(LogBitPVisitor<?> logBitPVisitor);

	LogBitPVisitor<?> logBitPVisitor();

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
	default boolean logTest(final IntegerStruct integer) {
		final IntegerStruct and = logAnd(integer);
		return !and.zerop();
	}

	abstract class GcdVisitor<S extends IntegerStruct> {

		final S integer1;

		GcdVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct gcd(IntIntegerStruct integer2);

		public abstract IntegerStruct gcd(LongIntegerStruct integer2);

		public abstract IntegerStruct gcd(BigIntegerStruct integer2);
	}

	abstract class LcmVisitor<S extends IntegerStruct> {

		final S integer1;

		LcmVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct lcm(IntIntegerStruct integer2);

		public abstract IntegerStruct lcm(LongIntegerStruct integer2);

		public abstract IntegerStruct lcm(BigIntegerStruct integer2);
	}

	abstract class AshVisitor<S extends IntegerStruct> {

		final S integer;

		AshVisitor(final S integer) {
			this.integer = integer;
		}

		public abstract IntegerStruct ash(IntIntegerStruct count);

		public abstract IntegerStruct ash(LongIntegerStruct count);

		public abstract IntegerStruct ash(BigIntegerStruct count);
	}

	abstract class LogAndVisitor<S extends IntegerStruct> {

		final S integer1;

		LogAndVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct logAnd(IntIntegerStruct integer2);

		public abstract IntegerStruct logAnd(LongIntegerStruct integer2);

		public abstract IntegerStruct logAnd(BigIntegerStruct integer2);
	}

	abstract class LogAndC1Visitor<S extends IntegerStruct> {

		final S integer1;

		LogAndC1Visitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct logAndC1(IntIntegerStruct integer2);

		public abstract IntegerStruct logAndC1(LongIntegerStruct integer2);

		public abstract IntegerStruct logAndC1(BigIntegerStruct integer2);
	}

	abstract class LogAndC2Visitor<S extends IntegerStruct> {

		final S integer1;

		LogAndC2Visitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct logAndC2(IntIntegerStruct integer2);

		public abstract IntegerStruct logAndC2(LongIntegerStruct integer2);

		public abstract IntegerStruct logAndC2(BigIntegerStruct integer2);
	}

	abstract class LogEqvVisitor<S extends IntegerStruct> {

		final S integer1;

		LogEqvVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct logEqv(IntIntegerStruct integer2);

		public abstract IntegerStruct logEqv(LongIntegerStruct integer2);

		public abstract IntegerStruct logEqv(BigIntegerStruct integer2);
	}

	abstract class LogIorVisitor<S extends IntegerStruct> {

		final S integer1;

		LogIorVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct logIor(IntIntegerStruct integer2);

		public abstract IntegerStruct logIor(LongIntegerStruct integer2);

		public abstract IntegerStruct logIor(BigIntegerStruct integer2);
	}

	abstract class LogNandVisitor<S extends IntegerStruct> {

		final S integer1;

		LogNandVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct logNand(IntIntegerStruct integer2);

		public abstract IntegerStruct logNand(LongIntegerStruct integer2);

		public abstract IntegerStruct logNand(BigIntegerStruct integer2);
	}

	abstract class LogNorVisitor<S extends IntegerStruct> {

		final S integer1;

		LogNorVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct logNor(IntIntegerStruct integer2);

		public abstract IntegerStruct logNor(LongIntegerStruct integer2);

		public abstract IntegerStruct logNor(BigIntegerStruct integer2);
	}

	abstract class LogOrC1Visitor<S extends IntegerStruct> {

		final S integer1;

		LogOrC1Visitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct logOrC1(IntIntegerStruct integer2);

		public abstract IntegerStruct logOrC1(LongIntegerStruct integer2);

		public abstract IntegerStruct logOrC1(BigIntegerStruct integer2);
	}

	abstract class LogOrC2Visitor<S extends IntegerStruct> {

		final S integer1;

		LogOrC2Visitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct logOrC2(IntIntegerStruct integer2);

		public abstract IntegerStruct logOrC2(LongIntegerStruct integer2);

		public abstract IntegerStruct logOrC2(BigIntegerStruct integer2);
	}

	abstract class LogXorVisitor<S extends IntegerStruct> {

		final S integer1;

		LogXorVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		public abstract IntegerStruct logXor(IntIntegerStruct integer2);

		public abstract IntegerStruct logXor(LongIntegerStruct integer2);

		public abstract IntegerStruct logXor(BigIntegerStruct integer2);
	}

	abstract class LogBitPVisitor<S extends IntegerStruct> {

		final S integer;

		LogBitPVisitor(final S integer) {
			this.integer = integer;
		}

		public abstract boolean logBitP(IntIntegerStruct index);

		public abstract boolean logBitP(LongIntegerStruct index);

		public abstract boolean logBitP(BigIntegerStruct index);
	}
}

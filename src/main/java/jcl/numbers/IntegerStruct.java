/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

import jcl.util.NumberUtils;
import org.apfloat.Apfloat;
import org.apfloat.Apint;

/**
 * The {@link IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public interface IntegerStruct extends RationalStruct {

	/**
	 * {@link IntegerStruct} constant representing 0.
	 */
	IntegerStruct ZERO = IntIntegerStruct.valueOf(0);

	/**
	 * {@link IntegerStruct} constant representing 1.
	 */
	IntegerStruct ONE = IntIntegerStruct.valueOf(1);

	/**
	 * {@link IntegerStruct} constant representing 2.
	 */
	IntegerStruct TWO = IntIntegerStruct.valueOf(2);

	/**
	 * {@link IntegerStruct} constant representing 10.
	 */
	IntegerStruct TEN = IntIntegerStruct.valueOf(10);

	/**
	 * {@link IntegerStruct} constant representing -1.
	 */
	IntegerStruct MINUS_ONE = IntIntegerStruct.valueOf(-1);

	/**
	 * Returns a new IntegerStruct representing the provided {@code int}. This will subclass appropriately to an {@link
	 * IntIntegerStruct}, which is the best data structure to hold an {@code int} value.
	 *
	 * @param i
	 * 		the {@link int} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link int}
	 */
	static IntegerStruct valueOf(final int i) {
		return IntIntegerStruct.valueOf(i);
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
	static IntegerStruct valueOf(final long l) {
		if (NumberUtils.longFitsInInt(l)) {
			return IntIntegerStruct.valueOf(NumberUtils.longToInt(l));
		}
		return LongIntegerStruct.valueOf(l);
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

	/**
	 * Returns a new IntegerStruct representing the provided {@link String}. This will subclass appropriately to the
	 * IntegerStruct implementation that would most accurately hold the integer data structure.
	 *
	 * @param s
	 * 		the {@link String} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link String}
	 */
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

	/**
	 * Returns this IntegerStruct as a {@code int} value.
	 *
	 * @return this IntegerStruct as a {@code int} value
	 */
	int intValue();

	/**
	 * Returns this IntegerStruct as a {@code long} value.
	 *
	 * @return this IntegerStruct as a {@code long} value
	 */
	long longValue();

	/**
	 * Returns this IntegerStruct as a {@link BigInteger} value.
	 *
	 * @return this IntegerStruct as a {@link BigInteger} value
	 */
	BigInteger bigIntegerValue();

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

	/**
	 * Returns the greatest common divisor for this IntegerStruct using the provided {@link GcdVisitor}.
	 *
	 * @param gcdVisitor
	 * 		the {@link GcdVisitor} to be used in the greatest common divisor operation
	 *
	 * @return the greatest common divisor for this IntegerStruct using the provided {@link GcdVisitor}
	 */
	IntegerStruct gcd(GcdVisitor<?> gcdVisitor);

	/**
	 * Returns a new {@link GcdVisitor} with this IntegerStruct to be used in a greatest common divisor operation.
	 *
	 * @return a new {@link GcdVisitor} with this IntegerStruct to be used in a greatest common divisor operation
	 */
	GcdVisitor<?> gcdVisitor();

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

	/**
	 * Returns the least common multiple for this IntegerStruct using the provided {@link LcmVisitor}.
	 *
	 * @param lcmVisitor
	 * 		the {@link LcmVisitor} to be used in the least common multiple operation
	 *
	 * @return the least common multiple for this IntegerStruct using the provided {@link LcmVisitor}
	 */
	IntegerStruct lcm(LcmVisitor<?> lcmVisitor);

	/**
	 * Returns a new {@link LcmVisitor} with this IntegerStruct to be used in a least common multiple operation.
	 *
	 * @return a new {@link LcmVisitor} with this IntegerStruct to be used in a least common multiple operation
	 */
	LcmVisitor<?> lcmVisitor();

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
	default IntegerStruct ash(final IntegerStruct count) {
		final AshVisitor<?> ashVisitor = ashVisitor();
		return count.ash(ashVisitor);
	}

	/**
	 * Returns the arithmetic shift operation on the binary representation of this IntegerStruct using the provided
	 * {@link AshVisitor}.
	 *
	 * @param ashVisitor
	 * 		the {@link AshVisitor} to be used in the arithmetic shift operation
	 *
	 * @return the arithmetic shift operation on the binary representation of this IntegerStruct using the provided
	 * {@link AshVisitor}
	 */
	IntegerStruct ash(AshVisitor<?> ashVisitor);

	/**
	 * Returns a new {@link AshVisitor} with this IntegerStruct to be used in an arithmetic shift operation.
	 *
	 * @return a new {@link AshVisitor} with this IntegerStruct to be used in an arithmetic shift operation
	 */
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

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct using the provided {@link LogAndVisitor}.
	 *
	 * @param logAndVisitor
	 * 		the {@link LogAndVisitor} to be used in the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct using the provided {@link LogAndVisitor}
	 */
	IntegerStruct logAnd(LogAndVisitor<?> logAndVisitor);

	/**
	 * Returns a new {@link LogAndVisitor} with this IntegerStruct to be used in a bit-wise logical 'and' operation.
	 *
	 * @return a new {@link LogAndVisitor} with this IntegerStruct to be used in a bit-wise logical 'and' operation
	 */
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

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct using the provided {@link LogAndC1Visitor}.
	 *
	 * @param logAndC1Visitor
	 * 		the {@link LogAndC1Visitor} to be used in the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct using the provided {@link LogAndC1Visitor}
	 */
	IntegerStruct logAndC1(LogAndC1Visitor<?> logAndC1Visitor);

	/**
	 * Returns a new {@link LogAndC1Visitor} with this IntegerStruct to be used in a bit-wise logical 'and' operation.
	 *
	 * @return a new {@link LogAndC1Visitor} with this IntegerStruct to be used in a bit-wise logical 'and' operation
	 */
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

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct using the provided {@link LogAndC2Visitor}.
	 *
	 * @param logAndC2Visitor
	 * 		the {@link LogAndC2Visitor} to be used in the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct using the provided {@link LogAndC2Visitor}
	 */
	IntegerStruct logAndC2(LogAndC2Visitor<?> logAndC2Visitor);

	/**
	 * Returns a new {@link LogAndC2Visitor} with this IntegerStruct to be used in a bit-wise logical 'and' operation.
	 *
	 * @return a new {@link LogAndC2Visitor} with this IntegerStruct to be used in a bit-wise logical 'and' operation
	 */
	LogAndC2Visitor<?> logAndC2Visitor();

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

	/**
	 * Returns the bit-wise logical 'equivalence', or 'exclusive-nor' of this IntegerStruct using the provided {@link
	 * LogEqvVisitor}.
	 *
	 * @param logEqvVisitor
	 * 		the {@link LogEqvVisitor} to be used in the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'equivalence', or 'exclusive-nor' of this IntegerStruct using the provided {@link
	 * LogEqvVisitor}
	 */
	IntegerStruct logEqv(LogEqvVisitor<?> logEqvVisitor);

	/**
	 * Returns a new {@link LogEqvVisitor} with this IntegerStruct to be used in a bit-wise logical 'equivalence', or
	 * 'exclusive-nor' operation.
	 *
	 * @return a new {@link LogEqvVisitor} with this IntegerStruct to be used in a bit-wise logical 'equivalence', or
	 * 'exclusive-nor' operation
	 */
	LogEqvVisitor<?> logEqvVisitor();

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

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct using the provided {@link LogIorVisitor}.
	 *
	 * @param logIorVisitor
	 * 		the {@link LogIorVisitor} to be used in the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct using the provided {@link LogIorVisitor}
	 */
	IntegerStruct logIor(LogIorVisitor<?> logIorVisitor);

	/**
	 * Returns a new {@link LogIorVisitor} with this IntegerStruct to be used in a bit-wise logical 'inclusive-or'
	 * operation.
	 *
	 * @return a new {@link LogIorVisitor} with this IntegerStruct to be used in a bit-wise logical 'inclusive-or'
	 * operation
	 */
	LogIorVisitor<?> logIorVisitor();

	/**
	 * Returns the bit-wise logical 'nand' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'nand' of this IntegerStruct and the provided IntegerStruct
	 */
	default IntegerStruct logNand(final IntegerStruct integer) {
		final LogNandVisitor<?> logNandVisitor = logNandVisitor();
		return integer.logNand(logNandVisitor);
	}

	/**
	 * Returns the bit-wise logical 'nand' of this IntegerStruct using the provided {@link LogNandVisitor}.
	 *
	 * @param logNandVisitor
	 * 		the {@link LogNandVisitor} to be used in the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'nand' of this IntegerStruct using the provided {@link LogNandVisitor}
	 */
	IntegerStruct logNand(LogNandVisitor<?> logNandVisitor);

	/**
	 * Returns a new {@link LogNandVisitor} with this IntegerStruct to be used in a bit-wise logical 'nand' operation.
	 *
	 * @return a new {@link LogNandVisitor} with this IntegerStruct to be used in a bit-wise logical 'and' operation
	 */
	LogNandVisitor<?> logNandVisitor();

	/**
	 * Returns the bit-wise logical 'nor' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'nor' of this IntegerStruct and the provided IntegerStruct
	 */
	default IntegerStruct logNor(final IntegerStruct integer) {
		final LogNorVisitor<?> logNorVisitor = logNorVisitor();
		return integer.logNor(logNorVisitor);
	}

	/**
	 * Returns the bit-wise logical 'nor' of this IntegerStruct using the provided {@link LogNorVisitor}.
	 *
	 * @param logNorVisitor
	 * 		the {@link LogNorVisitor} to be used in the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'nor' of this IntegerStruct using the provided {@link LogNorVisitor}
	 */
	IntegerStruct logNor(LogNorVisitor<?> logNorVisitor);

	/**
	 * Returns a new {@link LogNorVisitor} with this IntegerStruct to be used in a bit-wise logical 'nor' operation.
	 *
	 * @return a new {@link LogNorVisitor} with this IntegerStruct to be used in a bit-wise logical 'nor' operation
	 */
	LogNorVisitor<?> logNorVisitor();

	/**
	 * Returns the bit-wise logical 'not' of this IntegerStruct.
	 *
	 * @return the bit-wise logical 'not' of this IntegerStruct
	 */
	IntegerStruct logNot();

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
	default IntegerStruct logOrC1(final IntegerStruct integer) {
		final LogOrC1Visitor<?> logOrC1Visitor = logOrC1Visitor();
		return integer.logOrC1(logOrC1Visitor);
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct using the provided {@link LogOrC1Visitor}.
	 *
	 * @param logOrC1Visitor
	 * 		the {@link LogOrC1Visitor} to be used in the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct using the provided {@link LogOrC1Visitor}
	 */
	IntegerStruct logOrC1(LogOrC1Visitor<?> logOrC1Visitor);

	/**
	 * Returns a new {@link LogOrC1Visitor} with this IntegerStruct to be used in a bit-wise logical 'inclusive-or'
	 * operation.
	 *
	 * @return a new {@link LogOrC1Visitor} with this IntegerStruct to be used in a bit-wise logical 'inclusive-or'
	 * operation
	 */
	LogOrC1Visitor<?> logOrC1Visitor();

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct and the compliment of provided IntegerStruct
	 */
	default IntegerStruct logOrC2(final IntegerStruct integer) {
		final LogOrC2Visitor<?> logOrC2Visitor = logOrC2Visitor();
		return integer.logOrC2(logOrC2Visitor);
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct using the provided {@link LogOrC2Visitor}.
	 *
	 * @param logOrC2Visitor
	 * 		the {@link LogOrC2Visitor} to be used in the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct using the provided {@link LogOrC2Visitor}
	 */
	IntegerStruct logOrC2(LogOrC2Visitor<?> logOrC2Visitor);

	/**
	 * Returns a new {@link LogOrC2Visitor} with this IntegerStruct to be used in a bit-wise logical 'inclusive-or'
	 * operation.
	 *
	 * @return a new {@link LogOrC2Visitor} with this IntegerStruct to be used in a bit-wise logical 'inclusive-or'
	 * operation
	 */
	LogOrC2Visitor<?> logOrC2Visitor();

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

	/**
	 * Returns the bit-wise logical 'exclusive-or' of this IntegerStruct using the provided {@link LogXorVisitor}.
	 *
	 * @param logXorVisitor
	 * 		the {@link LogXorVisitor} to be used in the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of this IntegerStruct using the provided {@link LogXorVisitor}
	 */
	IntegerStruct logXor(LogXorVisitor<?> logXorVisitor);

	/**
	 * Returns a new {@link LogXorVisitor} with this IntegerStruct to be used in a bit-wise logical 'exclusive-or'
	 * operation.
	 *
	 * @return a new {@link LogXorVisitor} with this IntegerStruct to be used in a bit-wise logical 'exclusive-or'
	 * operation
	 */
	LogXorVisitor<?> logXorVisitor();

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

	/**
	 * Returns true if the bit in this IntegerStruct whose index is {@code index} is a one-bit using the provided
	 * {@link
	 * LogBitPVisitor}.
	 *
	 * @param logBitPVisitor
	 * 		the {@link LogBitPVisitor} to be used in the active bit test operation
	 *
	 * @return true if the bit in this IntegerStruct whose index is {@code index} is a one-bit using the provided {@link
	 * LogBitPVisitor}
	 */
	boolean logBitP(LogBitPVisitor<?> logBitPVisitor);

	/**
	 * Returns a new {@link LogBitPVisitor} with this IntegerStruct to be used in an active bit test operation.
	 *
	 * @return a new {@link LogBitPVisitor} with this IntegerStruct to be used in an active bit test' operation
	 */
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

	/**
	 * Returns the number of bits needed to represent this IntegerStruct in binary two's-complement format.
	 *
	 * @return the number of bits needed to represent this IntegerStruct in binary two's-complement format
	 */
	IntegerStruct integerLength();

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

	/*
		RationalStruct
	 */

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@code this} as the numerator.
	 */
	@Override
	default IntegerStruct numerator() {
		return this;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #ONE} as the denominator of IntegerStructs is always '1'.
	 */
	@Override
	default IntegerStruct denominator() {
		return ONE;
	}

	/*
		RealStruct
	 */

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
	default QuotientRemainderResult floor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
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
	default QuotientRemainderResult ffloor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
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
	default QuotientRemainderResult ceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
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
	default QuotientRemainderResult fceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
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
	default QuotientRemainderResult round(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
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
	default QuotientRemainderResult fround(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fround(this);
	}

	@Override
	default QuotientRemainderVisitor<?> quotientRemainderVisitor() {
		return new RationalQuotientRemainderVisitor<>(this);
	}

	/*
		NumberStruct
	 */

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the exponential function result for this IntegerStruct as this {@code base} and the provided {@link
	 * NumberStruct} as the {@code power}. If {@code power} is '0' and power is an IntegerStruct, {@link #ONE} is
	 * returned. If {@code power} is '0' and power is not an IntegerStruct, {@link FloatStruct#ONE} is returned. If
	 * this IntegerStruct is either '0' or '1', {@code this} is returned.
	 */
	@Override
	default NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			if (power instanceof IntegerStruct) {
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

	// Visitor Implementations

	/**
	 * {@link GcdVisitor} for computing greatest common divisor for {@link IntegerStruct}s.
	 */
	abstract class GcdVisitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the greatest common divisor operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the greatest common divisor operation
		 */
		GcdVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the greatest common divisor for an {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the greatest common divisor operation
		 *
		 * @return the greatest common divisor result
		 */
		public abstract IntegerStruct gcd(IntIntegerStruct integer2);

		/**
		 * Computes the greatest common divisor for an {@link S} and a {@link LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the greatest common divisor operation
		 *
		 * @return the greatest common divisor result
		 */
		public abstract IntegerStruct gcd(LongIntegerStruct integer2);

		/**
		 * Computes the greatest common divisor for an {@link S} and a {@link BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the greatest common divisor operation
		 *
		 * @return the greatest common divisor result
		 */
		public abstract IntegerStruct gcd(BigIntegerStruct integer2);
	}

	/**
	 * {@link LcmVisitor} for computing least common multiple for {@link IntegerStruct}s.
	 */
	abstract class LcmVisitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the least common multiple operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the least common multiple operation
		 */
		LcmVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the least common multiple for an {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the least common multiple operation
		 *
		 * @return the least common multiple result
		 */
		public abstract IntegerStruct lcm(IntIntegerStruct integer2);

		/**
		 * Computes the least common multiple for an {@link S} and a {@link LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the least common multiple operation
		 *
		 * @return the least common multiple result
		 */
		public abstract IntegerStruct lcm(LongIntegerStruct integer2);

		/**
		 * Computes the least common multiple for an {@link S} and a {@link BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the least common multiple operation
		 *
		 * @return the least common multiple result
		 */
		public abstract IntegerStruct lcm(BigIntegerStruct integer2);
	}

	/**
	 * {@link AshVisitor} for performing the arithmetic shift operation on the binary representation for {@link
	 * IntegerStruct}s.
	 */
	abstract class AshVisitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the {@link IntegerStruct} to shift.
		 */
		final S integer;

		/**
		 * Package private constructor.
		 *
		 * @param integer
		 * 		the {@link IntegerStruct} to shift
		 */
		AshVisitor(final S integer) {
			this.integer = integer;
		}

		/**
		 * Performs the arithmetic shift operation on the binary representation for an {@link S} with an {@link
		 * IntIntegerStruct} for bit shifting.
		 *
		 * @param count
		 * 		the bit positions to shift an {@link IntegerStruct} left or right
		 *
		 * @return the arithmetic shift result
		 */
		public abstract IntegerStruct ash(IntIntegerStruct count);

		/**
		 * Performs the arithmetic shift operation on the binary representation for an {@link S} with a {@link
		 * LongIntegerStruct} for bit shifting.
		 *
		 * @param count
		 * 		the bit positions to shift an {@link IntegerStruct} left or right
		 *
		 * @return the arithmetic shift result
		 */
		public abstract IntegerStruct ash(LongIntegerStruct count);

		/**
		 * Performs the arithmetic shift operation on the binary representation for an {@link S} with a {@link
		 * BigIntegerStruct} for bit shifting.
		 *
		 * @param count
		 * 		the bit positions to shift an {@link IntegerStruct} left or right
		 *
		 * @return the arithmetic shift result
		 */
		public abstract IntegerStruct ash(BigIntegerStruct count);
	}

	/**
	 * {@link LogAndVisitor} for computing bit-wise logical 'and' for {@link IntegerStruct}s.
	 */
	abstract class LogAndVisitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the bit-wise logical operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the bit-wise logical operation
		 */
		LogAndVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the bit-wise logical 'and' for an {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logAnd(IntIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'and' for an {@link S} and a {@link LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logAnd(LongIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'and' for an {@link S} and a {@link BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logAnd(BigIntegerStruct integer2);
	}

	/**
	 * {@link LogAndC1Visitor} for computing bit-wise logical 'and' for an {@link IntegerStruct} and the complement of
	 * another {@link IntegerStruct}.
	 */
	abstract class LogAndC1Visitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the bit-wise logical operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the bit-wise logical operation
		 */
		LogAndC1Visitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the bit-wise logical 'and' for the complement of {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logAndC1(IntIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'and' for the complement of {@link S} and a {@link LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logAndC1(LongIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'and' for the complement of {@link S} and a {@link BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logAndC1(BigIntegerStruct integer2);
	}

	/**
	 * {@link LogAndC2Visitor} for computing bit-wise logical 'and' for the complement of an {@link IntegerStruct} and
	 * another {@link IntegerStruct}.
	 */
	abstract class LogAndC2Visitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the bit-wise logical operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the bit-wise logical operation
		 */
		LogAndC2Visitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the bit-wise logical 'and' for an {@link S} and the complement of an {@link IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logAndC2(IntIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'and' for an {@link S} and the complement of a {@link LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logAndC2(LongIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'and' for an {@link S} and the complement of a {@link BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logAndC2(BigIntegerStruct integer2);
	}

	/**
	 * {@link LogEqvVisitor} for computing bit-wise logical 'equivalence', or 'exclusive-nor' for {@link
	 * IntegerStruct}s.
	 */
	abstract class LogEqvVisitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the bit-wise logical operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the bit-wise logical operation
		 */
		LogEqvVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the bit-wise logical 'equivalence', or 'exclusive-nor' for an {@link S} and an {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logEqv(IntIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'equivalence', or 'exclusive-nor' for an {@link S} and a {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logEqv(LongIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'equivalence', or 'exclusive-nor' for an {@link S} and a {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logEqv(BigIntegerStruct integer2);
	}

	/**
	 * {@link LogIorVisitor} for computing bit-wise logical 'inclusive-or' for {@link IntegerStruct}s.
	 */
	abstract class LogIorVisitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the bit-wise logical operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the bit-wise logical operation
		 */
		LogIorVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the bit-wise logical 'inclusive-or' for an {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logIor(IntIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'inclusive-or' for an {@link S} and a {@link LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logIor(LongIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'inclusive-or' for an {@link S} and a {@link BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logIor(BigIntegerStruct integer2);
	}

	/**
	 * {@link LogNandVisitor} for computing bit-wise logical 'nand' for {@link IntegerStruct}s.
	 */
	abstract class LogNandVisitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the bit-wise logical operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the bit-wise logical operation
		 */
		LogNandVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the bit-wise logical 'nand' for an {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logNand(IntIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'nand' for an {@link S} and a {@link LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logNand(LongIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'nand' for an {@link S} and a {@link BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logNand(BigIntegerStruct integer2);
	}

	/**
	 * {@link LogNorVisitor} for computing bit-wise logical 'nor' for {@link IntegerStruct}s.
	 */
	abstract class LogNorVisitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the bit-wise logical operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the bit-wise logical operation
		 */
		LogNorVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the bit-wise logical 'nor' for an {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logNor(IntIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'nor' for an {@link S} and a {@link LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logNor(LongIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'nor' for an {@link S} and a {@link BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logNor(BigIntegerStruct integer2);
	}

	/**
	 * {@link LogOrC1Visitor} for computing bit-wise logical 'inclusive-or' for an {@link IntegerStruct} and the
	 * complement of
	 * another {@link IntegerStruct}.
	 */
	abstract class LogOrC1Visitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the bit-wise logical operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the bit-wise logical operation
		 */
		LogOrC1Visitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the bit-wise logical 'inclusive-or' for the complement of {@link S} and an {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logOrC1(IntIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'inclusive-or' for the complement of {@link S} and a {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logOrC1(LongIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'inclusive-or' for the complement of {@link S} and a {@link BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logOrC1(BigIntegerStruct integer2);
	}

	/**
	 * {@link LogOrC2Visitor} for computing bit-wise logical 'inclusive-or' for the complement of an {@link
	 * IntegerStruct} and another {@link IntegerStruct}.
	 */
	abstract class LogOrC2Visitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the bit-wise logical operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the bit-wise logical operation
		 */
		LogOrC2Visitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the bit-wise logical 'inclusive-or' for an {@link S} and the complement of an {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logOrC2(IntIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'inclusive-or' for an {@link S} and the complement of a {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logOrC2(LongIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'inclusive-or' for an {@link S} and the complement of a {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logOrC2(BigIntegerStruct integer2);
	}

	/**
	 * {@link LogXorVisitor} for computing bit-wise logical 'exclusive-or' for {@link IntegerStruct}s.
	 */
	abstract class LogXorVisitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the first argument in the bit-wise logical operation.
		 */
		final S integer1;

		/**
		 * Package private constructor.
		 *
		 * @param integer1
		 * 		the first argument in the bit-wise logical operation
		 */
		LogXorVisitor(final S integer1) {
			this.integer1 = integer1;
		}

		/**
		 * Computes the bit-wise logical 'exclusive-or' for an {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logXor(IntIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'exclusive-or' for an {@link S} and a {@link LongIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logXor(LongIntegerStruct integer2);

		/**
		 * Computes the bit-wise logical 'exclusive-or' for an {@link S} and a {@link BigIntegerStruct}.
		 *
		 * @param integer2
		 * 		the second argument in the bit-wise logical operation
		 *
		 * @return the bit-wise logical result
		 */
		public abstract IntegerStruct logXor(BigIntegerStruct integer2);
	}

	/**
	 * {@link LogBitPVisitor} for active bit testing for {@link IntegerStruct}s.
	 */
	abstract class LogBitPVisitor<S extends IntegerStruct> {

		/**
		 * The {@link S} as the {@link IntegerStruct} for active bit testing.
		 */
		final S integer;

		/**
		 * Package private constructor.
		 *
		 * @param integer
		 * 		the {@link IntegerStruct} for active bit testing
		 */
		LogBitPVisitor(final S integer) {
			this.integer = integer;
		}

		/**
		 * Returns true if the bit whose index is {@code index} is a one-bit; otherwise, returns false.
		 *
		 * @param index
		 * 		the index value to test for a one-bit
		 *
		 * @return true if the bit whose index is {@code index} is a one-bit; otherwise, false
		 */
		public abstract boolean logBitP(IntIntegerStruct index);

		/**
		 * Returns true if the bit whose index is {@code index} is a one-bit; otherwise, returns false.
		 *
		 * @param index
		 * 		the index value to test for a one-bit
		 *
		 * @return true if the bit whose index is {@code index} is a one-bit; otherwise, false
		 */
		public abstract boolean logBitP(LongIntegerStruct index);

		/**
		 * Returns true if the bit whose index is {@code index} is a one-bit; otherwise, returns false.
		 *
		 * @param index
		 * 		the index value to test for a one-bit
		 *
		 * @return true if the bit whose index is {@code index} is a one-bit; otherwise, false
		 */
		public abstract boolean logBitP(BigIntegerStruct index);
	}

	/*
		Deprecated
	 */

	@Deprecated
	static IntegerStruct valueOf(final Apfloat apfloat) {
		return IntIntegerStruct.valueOf(apfloat.intValue());
	}

	@Deprecated
	@Override
	default BigDecimal bigDecimalValue() {
		return NumberUtils.bigDecimalValue(bigIntegerValue());
	}

	@Deprecated
	@Override
	default Apfloat apfloatValue() {
		return new Apint(bigIntegerValue());
	}
}

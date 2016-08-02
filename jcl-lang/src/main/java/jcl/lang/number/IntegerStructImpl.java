/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.number;

import java.math.BigInteger;
import java.util.List;

import jcl.lang.NumberStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import jcl.lang.statics.PrinterVariables;
import jcl.type.IntegerType;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.ApintMath;
import org.apfloat.Aprational;

/**
 * The {@link IntegerStructImpl} is the object representation of a Lisp 'integer' type.
 */
public final class IntegerStructImpl extends RationalStructImpl<Apint> {

	/**
	 * {@link IntegerStructImpl} constant representing 0.
	 */
	public static final IntegerStructImpl ZERO = valueOf(0);

	/**
	 * {@link IntegerStructImpl} constant representing 1.
	 */
	public static final IntegerStructImpl ONE = valueOf(1);

	/**
	 * {@link IntegerStructImpl} constant representing 2.
	 */
	public static final IntegerStructImpl TWO = valueOf(2);

	/**
	 * {@link IntegerStructImpl} constant representing 10.
	 */
	public static final IntegerStructImpl TEN = valueOf(10);

	/**
	 * {@link IntegerStructImpl} constant representing -1.
	 */
	public static final IntegerStructImpl MINUS_ONE = valueOf(-1);

	/**
	 * {@link Apint} constant for calculating whether an {@link IntegerStructImpl} is even or odd.
	 */
	private static final Apint APINT_2 = new Apint(2);

	/**
	 * Private constructor.
	 *
	 * @param apint
	 * 		the value of the IntegerStruct
	 */
	private IntegerStructImpl(final Apint apint) {
		super(IntegerType.INSTANCE, apint);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@link Integer}.
	 *
	 * @param i
	 * 		the {@link Integer} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link Integer}
	 */
	public static IntegerStructImpl valueOf(final Integer i) {
		final Apint apint = new Apint(i);
		return valueOf(apint);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@link Long}.
	 *
	 * @param l
	 * 		the {@link Long} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link Long}
	 */
	public static IntegerStructImpl valueOf(final Long l) {
		final Apint apint = new Apint(l);
		return valueOf(apint);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@link BigInteger}.
	 *
	 * @param bigInteger
	 * 		the {@link BigInteger} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link BigInteger}
	 */
	public static IntegerStructImpl valueOf(final BigInteger bigInteger) {
		final Apint apint = new Apint(bigInteger);
		return valueOf(apint);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@link String}.
	 *
	 * @param s
	 * 		the {@link String} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link String}
	 */
	public static IntegerStructImpl valueOf(final String s) {
		final Apint apint = new Apint(s);
		return valueOf(apint);
	}

	/**
	 * Returns a IntegerStruct object with the provided {@link Apint} value.
	 *
	 * @param apint
	 * 		the {@link Apint} value of the resulting IntegerStruct
	 *
	 * @return a IntegerStruct object with the provided {@link Apint} value
	 */
	public static IntegerStructImpl valueOf(final Apint apint) {
		return new IntegerStructImpl(apint);
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
	public static IntegerStructImpl gcd(final List<IntegerStructImpl> integers) {
		return integers.stream().reduce(ZERO, IntegerStructImpl::gcd);
	}

	/**
	 * Returns the greatest common divisor between this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct in comparison to this IntegerStruct to determine the greatest common divisor
	 *
	 * @return the greatest common divisor between this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStructImpl gcd(final IntegerStructImpl integer) {
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
	public static IntegerStructImpl lcm(final List<IntegerStructImpl> integers) {
		return integers.stream().reduce(ONE, IntegerStructImpl::lcm);
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
	public IntegerStructImpl lcm(final IntegerStructImpl integer) {
		final Apint lcm = ApintMath.lcm(ap, integer.ap);
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
	public IntegerStructImpl ash(final IntegerStructImpl count) {
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
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStructImpl logAnd(final IntegerStructImpl integer) {
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
	public static IntegerStructImpl logAnd(final List<IntegerStructImpl> integers) {
		return integers.stream().reduce(MINUS_ONE, IntegerStructImpl::logAnd);
	}

	/**
	 * Returns the bit-wise logical 'and' of the compliment of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStructImpl logAndC1(final IntegerStructImpl integer) {
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
	public IntegerStructImpl logAndC2(final IntegerStructImpl integer) {
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
	public static IntegerStructImpl logEqv(final List<IntegerStructImpl> integers) {
		return integers.stream().reduce(MINUS_ONE, IntegerStructImpl::logEqv);
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
	public IntegerStructImpl logEqv(final IntegerStructImpl integer) {
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
	public static IntegerStructImpl logIor(final List<IntegerStructImpl> integers) {
		return integers.stream().reduce(ZERO, IntegerStructImpl::logIor);
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStructImpl logIor(final IntegerStructImpl integer) {
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
	public IntegerStructImpl logNand(final IntegerStructImpl integer) {
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
	public IntegerStructImpl logNor(final IntegerStructImpl integer) {
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
	public IntegerStructImpl logNot() {
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
	public IntegerStructImpl logOrC1(final IntegerStructImpl integer) {
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
	public IntegerStructImpl logOrC2(final IntegerStructImpl integer) {
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
	public static IntegerStructImpl logXor(final List<IntegerStructImpl> integers) {
		return integers.stream().reduce(ZERO, IntegerStructImpl::logXor);
	}

	/**
	 * Returns the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	public IntegerStructImpl logXor(final IntegerStructImpl integer) {
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
	public boolean logBitP(final IntegerStructImpl index) {
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
	public IntegerStructImpl logCount() {
		final BigInteger bigInteger = ap.toBigInteger();
		final int bitCount = bigInteger.bitCount();
		return valueOf(bitCount);
	}

	/**
	 * Returns true if any of the bits designated by the 1's in this IntegerStruct are 1 in the provided
	 * IntegerStruct; otherwise, returns false.
	 *
	 * @param integer
	 * 		the IntegerStruct used in the test comparison to this IntegerStruct
	 *
	 * @return true if any of the bits designated by the 1's in this IntegerStruct are 1 in the provided
	 * IntegerStruct; otherwise, false.
	 */
	public boolean logTest(final IntegerStructImpl integer) {
		final IntegerStructImpl and = logAnd(integer);
		return and.ap.signum() != 0;
	}

	/**
	 * Returns the number of bits needed to represent this IntegerStruct in binary two's-complement format.
	 *
	 * @return the number of bits needed to represent this IntegerStruct in binary two's-complement format
	 */
	public IntegerStructImpl integerLength() {
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
		return Apcomplex.ZERO.equals(ap.mod(APINT_2));
	}

	/**
	 * Returns true if this IntegerStruct is odd (not divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct is odd (not divisible by two); otherwise, false
	 */
	public boolean oddp() {
		return !evenp();
	}

	/**
	 * Returns the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root.
	 *
	 * @return the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root
	 */
	public IntegerStructImpl isqrt() {
		final Apint[] sqrt = ApintMath.sqrt(ap);
		return valueOf(sqrt[0]);
	}

	/*
		RationalStruct
	 */

	@Override
	public IntegerStructImpl numerator() {
		return this;
	}

	@Override
	public IntegerStructImpl denominator() {
		return ONE;
	}

	/*
		RealStruct
	 */

	@Override
	public IntegerStructImpl rational() {
		return this;
	}

	@Override
	protected RealStruct getRemainderReal(final RealStruct divisor, final Apfloat remainder) {
		if (divisor instanceof IntegerStructImpl) {
			return valueOf((Apint) remainder);
		}
		return super.getRemainderReal(divisor, remainder);
	}

	/*
		NumberStruct
	 */

	@Override
	public Apint ap() {
		return ap;
	}

	@Override
	public IntegerStructImpl abs() {
		final Apint abs = ApintMath.abs(ap);
		return valueOf(abs);
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apint) {
			final Apint add = ap.add((Apint) numberAp);
			return valueOf(add);
		}
		return super.add(number);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apint) {
			final Apint subtract = ap.subtract((Apint) numberAp);
			return valueOf(subtract);
		}
		return super.subtract(number);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apint) {
			final Apint multiply = ap.multiply((Apint) numberAp);
			return valueOf(multiply);
		}
		return super.multiply(number);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apint) {
			final Apint divide = ap.divide((Apint) numberAp);
			return valueOf(divide);
		}
		return super.divide(number);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apint) {
			final Apint integerAp = (Apint) numberAp;
			final boolean shouldReverseCompare = ap.preferCompare(integerAp);
			return shouldReverseCompare ? integerAp.equals(ap) : ap.equals(integerAp);
		}
		return super.isEqualTo(number);
	}

	@Override
	public IntegerStructImpl realPart() {
		return this;
	}

	@Override
	public IntegerStructImpl conjugate() {
		return this;
	}

	@Override
	public IntegerStructImpl negation() {
		final Apint negate = ap.negate();
		return valueOf(negate);
	}

	@Override
	public RationalStruct reciprocal() {
		final Aprational reciprocal = new Aprational(Apcomplex.ONE, ap);
		return RationalStruct.valueOf(reciprocal);
	}

	/*
		ToString
	 */

	/**
	 * Int constant for the value '2'.
	 */
	private static final int TWO_PRINTER = 2;

	/**
	 * Int constant for the value '8'.
	 */
	private static final int EIGHT_PRINTER = 8;

	/**
	 * Int constant for the value '10'.
	 */
	private static final int TEN_PRINTER = 10;

	/**
	 * Int constant for the value '16'.
	 */
	private static final int SIXTEEN_PRINTER = 16;

	@Override
	public String toString() {
		final boolean printRadix = PrinterVariables.PRINT_RADIX.getVariableValue().booleanValue();
		final int printBase = PrinterVariables.PRINT_BASE.getVariableValue().intValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printRadix) {
			if (printBase == TWO_PRINTER) {
				stringBuilder.append("#b");
			} else if (printBase == EIGHT_PRINTER) {
				stringBuilder.append("#o");
			} else if (printBase == SIXTEEN_PRINTER) {
				stringBuilder.append("#x");
			} else {
				stringBuilder.append('#');
				stringBuilder.append(printBase);
				stringBuilder.append('r');
			}
		}

		final BigInteger bigInteger = bigIntegerValue();
		stringBuilder.append(bigInteger.toString(printBase));

		if (printRadix && (printBase == TEN_PRINTER)) {
			stringBuilder.append('.');
		}

		return stringBuilder.toString();
	}
}

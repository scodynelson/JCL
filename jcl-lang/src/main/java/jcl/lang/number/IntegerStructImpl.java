/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.number;

import java.math.BigInteger;

import jcl.lang.IntegerStruct;
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
public final class IntegerStructImpl extends RationalStructImpl<Apint> implements IntegerStruct {

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
	public static IntegerStruct valueOf(final Integer i) {
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
	public static IntegerStruct valueOf(final Long l) {
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
	public static IntegerStruct valueOf(final BigInteger bigInteger) {
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
	public static IntegerStruct valueOf(final String s) {
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
	public static IntegerStruct valueOf(final Apint apint) {
		return new IntegerStructImpl(apint);
	}

	@Override
	public int intValue() {
		return ap.intValue();
	}

	@Override
	public long longValue() {
		return ap.longValue();
	}

	@Override
	public BigInteger bigIntegerValue() {
		return ap.toBigInteger();
	}

	@Override
	public IntegerStruct gcd(final IntegerStruct integer) {
		final Apint gcd = ApintMath.gcd(ap, integer.ap());
		return valueOf(gcd);
	}

	@Override
	public IntegerStruct lcm(final IntegerStruct integer) {
		final Apint lcm = ApintMath.lcm(ap, integer.ap());
		return valueOf(lcm);
	}

	@Override
	public IntegerStruct ash(final IntegerStruct count) {
		final Apint countAp = count.ap();
		if (countAp.signum() == 0) {
			return this;
		}
		final int countI = countAp.intValue();

		// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
		final BigInteger shiftedBigInteger = ap.toBigInteger().shiftLeft(countI);
		return valueOf(shiftedBigInteger);
	}

	@Override
	public IntegerStruct logAnd(final IntegerStruct integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap().toBigInteger();
		return valueOf(bigInteger1.and(bigInteger2));
	}

	@Override
	public IntegerStruct logAndC1(final IntegerStruct integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap().toBigInteger();
		return valueOf(bigInteger1.not().and(bigInteger2));
	}

	@Override
	public IntegerStruct logAndC2(final IntegerStruct integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap().toBigInteger();
		return valueOf(bigInteger1.and(bigInteger2.not()));
	}

	@Override
	public IntegerStruct logEqv(final IntegerStruct integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap().toBigInteger();
		final BigInteger xor = bigInteger1.xor(bigInteger2);
		return valueOf(xor.not());
	}

	@Override
	public IntegerStruct logIor(final IntegerStruct integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap().toBigInteger();
		return valueOf(bigInteger1.or(bigInteger2));
	}

	@Override
	public IntegerStruct logNand(final IntegerStruct integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap().toBigInteger();
		final BigInteger and = bigInteger1.and(bigInteger2);
		return valueOf(and.not());
	}

	@Override
	public IntegerStruct logNor(final IntegerStruct integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap().toBigInteger();
		final BigInteger or = bigInteger1.or(bigInteger2);
		return valueOf(or.not());
	}

	@Override
	public IntegerStruct logNot() {
		final BigInteger bigInteger = ap.toBigInteger();
		return valueOf(bigInteger.not());
	}

	@Override
	public IntegerStruct logOrC1(final IntegerStruct integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap().toBigInteger();
		return valueOf(bigInteger1.not().or(bigInteger2));
	}

	@Override
	public IntegerStruct logOrC2(final IntegerStruct integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap().toBigInteger();
		return valueOf(bigInteger1.or(bigInteger2.not()));
	}

	@Override
	public IntegerStruct logXor(final IntegerStruct integer) {
		final BigInteger bigInteger1 = ap.toBigInteger();
		final BigInteger bigInteger2 = integer.ap().toBigInteger();
		return valueOf(bigInteger1.xor(bigInteger2));
	}

	@Override
	public boolean logBitP(final IntegerStruct index) {
		final BigInteger bigInteger = ap.toBigInteger();
		final int indexInt = index.intValue();
		return bigInteger.testBit(indexInt);
	}

	@Override
	public IntegerStruct logCount() {
		final BigInteger bigInteger = ap.toBigInteger();
		final int bitCount = bigInteger.bitCount();
		return valueOf(bitCount);
	}

	@Override
	public boolean logTest(final IntegerStruct integer) {
		final IntegerStruct and = logAnd(integer);
		return and.ap().signum() != 0;
	}

	@Override
	public IntegerStruct integerLength() {
		final BigInteger bigInteger = ap.toBigInteger();
		final int bitLength = bigInteger.bitLength();
		return valueOf(bitLength);
	}

	@Override
	public boolean evenp() {
		return Apcomplex.ZERO.equals(ap.mod(APINT_2));
	}

	@Override
	public IntegerStruct isqrt() {
		final Apint[] sqrt = ApintMath.sqrt(ap);
		return valueOf(sqrt[0]);
	}

	/*
		RationalStruct
	 */

	@Override
	public IntegerStruct numerator() {
		return this;
	}

	/*
		RealStruct
	 */

	@Override
	public IntegerStruct rational() {
		return this;
	}

	@Override
	protected RealStruct getRemainderReal(final RealStruct divisor, final Apfloat remainder) {
		if (divisor instanceof IntegerStruct) {
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
	public IntegerStruct abs() {
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
	public IntegerStruct realPart() {
		return this;
	}

	@Override
	public IntegerStruct conjugate() {
		return this;
	}

	@Override
	public IntegerStruct negation() {
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

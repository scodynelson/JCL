/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.List;

import org.apfloat.Apfloat;
import org.apfloat.Apint;

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
	default BigInteger getBigInteger() {
		return bigIntegerValue();
	}

	@Deprecated
	static IntegerStruct valueOf(final Apfloat apfloat) {
		return IntIntegerStruct.valueOf(apfloat.intValue());
	}

	@Deprecated
	@Override
	default BigDecimal bigDecimalValue() {
		return new BigDecimal(bigIntegerValue(), 1).multiply(BigDecimal.TEN);
	}

	@Deprecated
	@Override
	default Apfloat apfloatValue() {
		return new Apint(bigIntegerValue());
	}

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
	 * {@inheritDoc}
	 * <p>
	 * Determines the whether or not the numerical value of this IntegerStruct is zero, positive, or negative,
	 * returning {@code this}, {@link #ONE}, or {@link #MINUS_ONE} respectively.
	 */
	@Override
	default NumberStruct signum() {
		if (zerop()) {
			return this;
		} else if (plusp()) {
			return ONE;
		} else {
			return MINUS_ONE;
		}
	}

	@Override
	default RealStruct imagPart() {
		return ZERO;
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
		return new IntegerQuotientRemainderVisitor(this);
	}

	/**
	 * {@link IntegerQuotientRemainderVisitor} for computing quotient and remainder results for {@link
	 * IntegerStruct}s.
	 */
	final class IntegerQuotientRemainderVisitor extends RationalStruct.RationalQuotientRemainderVisitor<IntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerQuotientRemainderVisitor with the provided
		 * {@link IntegerStruct}.
		 *
		 * @param real
		 * 		the real argument in the computational quotient and remainder operation
		 */
		private IntegerQuotientRemainderVisitor(final IntegerStruct real) {
			super(real);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final IntegerStruct divisor, final RoundingMode roundingMode, final boolean isQuotientFloat) {
			final BigDecimal realBigDecimal = real.bigDecimalValue();
			final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();

			final BigDecimal quotient = realBigDecimal.divide(divisorBigDecimal, 0, roundingMode);
			final BigDecimal remainder = realBigDecimal.subtract(divisorBigDecimal.multiply(quotient));

			final RealStruct quotientReal;
			if (isQuotientFloat) {
				quotientReal = FloatStruct.valueOf(quotient);
			} else {
				final BigInteger quotientBigInteger = quotient.toBigInteger();
				quotientReal = IntegerStruct.valueOf(quotientBigInteger);
			}

			final BigInteger remainderBigInteger = remainder.toBigInteger();
			final IntegerStruct remainderInteger = IntegerStruct.valueOf(remainderBigInteger);

			return new QuotientRemainderResult(quotientReal, remainderInteger);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final FloatStruct divisor, final RoundingMode roundingMode, final boolean isQuotientFloat) {
			final BigDecimal realBigDecimal = real.bigDecimalValue();
			final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();

			final BigDecimal quotient = realBigDecimal.divide(divisorBigDecimal, 0, roundingMode);
			final BigDecimal remainder = realBigDecimal.subtract(divisorBigDecimal.multiply(quotient));

			final RealStruct quotientReal;
			if (isQuotientFloat) {
				quotientReal = getFloatQuotient(divisor, quotient);
			} else {
				final BigInteger quotientBigInteger = quotient.toBigInteger();
				quotientReal = IntegerStruct.valueOf(quotientBigInteger);
			}

			final FloatStruct remainderFloat = FloatStruct.valueOf(remainder);
			return new QuotientRemainderResult(quotientReal, remainderFloat);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final RatioStruct divisor, final RoundingMode roundingMode, final boolean isQuotientFloat) {
			final IntegerStruct rationalNumerator = real.numerator();
			final BigInteger rationalNumeratorBigInteger = rationalNumerator.getBigInteger();
			final IntegerStruct rationalDenominator = real.denominator();
			final BigInteger rationalDenominatorBigInteger = rationalDenominator.getBigInteger();

			final IntegerStruct divisorNumerator = divisor.numerator();
			final BigInteger divisorNumeratorBigInteger = divisorNumerator.getBigInteger();
			final IntegerStruct divisorDenominator = divisor.denominator();
			final BigInteger divisorDenominatorBigInteger = divisorDenominator.getBigInteger();

			// Invert and multiply
			final BigInteger multipliedNumerator = rationalNumeratorBigInteger.multiply(divisorDenominatorBigInteger);
			final BigInteger multipliedDenominator = rationalDenominatorBigInteger.multiply(divisorNumeratorBigInteger);

			// Divide to get quotient
			final BigDecimal multipliedNumeratorBigDecimal = new BigDecimal(multipliedNumerator);
			final BigDecimal multipliedDenominatorBigDecimal = new BigDecimal(multipliedDenominator);
			final BigDecimal quotient = multipliedNumeratorBigDecimal.divide(multipliedDenominatorBigDecimal, 0, roundingMode);
			final BigInteger quotientBigInteger = quotient.toBigInteger();

			final RealStruct quotientReal;
			if (isQuotientFloat) {
				quotientReal = FloatStruct.valueOf(quotient);
			} else {
				quotientReal = IntegerStruct.valueOf(quotientBigInteger);
			}

			// Multiply divisor by quotient
			final BigInteger multiply = divisorNumeratorBigInteger.multiply(quotientBigInteger);
			final RationalStruct product = RationalStruct.makeRational(multiply, divisorDenominatorBigInteger);

			// Subtract to get remainder: (Rational - Rational) produces Rational
			final NumberStruct remainder = real.subtract(product);

			return new QuotientRemainderResult(quotientReal, (RealStruct) remainder);
		}
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

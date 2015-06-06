/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.Objects;

import jcl.LispStruct;
import jcl.types.IntegerType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apache.commons.math3.util.FastMath;

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

	@Override
	public RealStruct abs() {
		if (bigInteger.signum() >= 0) {
			return this;
		}
		return new IntegerStruct(bigInteger.negate());
	}

	@Override
	public boolean zerop() {
		return bigInteger.signum() == 0;
	}

	@Override
	public boolean plusp() {
		return bigInteger.signum() > 0;
	}

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
	public NumberStruct add(final NumberStruct number) {
		return IntegerAddStrategy.INSTANCE.add(this, number);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		return IntegerSubtractStrategy.INSTANCE.subtract(this, number);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		return IntegerMultiplyStrategy.INSTANCE.multiply(this, number);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		return IntegerDivideStrategy.INSTANCE.divide(this, number);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		return IntegerEqualToStrategy.INSTANCE.equalTo(this, number);
	}

	@Override
	public boolean isLessThan(final RealStruct real) {
		return IntegerLessThanStrategy.INSTANCE.lessThan(this, real);
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		return IntegerGreaterThanStrategy.INSTANCE.greaterThan(this, real);
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		return IntegerLessThanOrEqualToStrategy.INSTANCE.lessThanOrEqualTo(this, real);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		return IntegerGreaterThanOrEqualToStrategy.INSTANCE.greaterThanOrEqualTo(this, real);
	}

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

	@Override
	public RealStruct imagPart() {
		return ZERO;
	}

	@Override
	public NumberStruct negation() {
		return new IntegerStruct(bigInteger.negate());
	}

	@Override
	public NumberStruct reciprocal() {
		return makeRational(BigInteger.ONE, bigInteger);
	}

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

		return IntegerExptStrategy.INSTANCE.expt(this, power);
	}

	/**
	 * Returns the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root.
	 *
	 * @return the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root
	 */
	public IntegerStruct isqrt() {
		final double doubleValue = doubleValue();
		final double sqrt = FastMath.sqrt(doubleValue);
		final double isqrt = FastMath.floor(sqrt);
		final BigDecimal isqrtBigDecimal = new BigDecimal(isqrt);
		final BigInteger isqrtBigInteger = isqrtBigDecimal.toBigInteger();
		return new IntegerStruct(isqrtBigInteger);
	}

	@Override
	public double doubleValue() {
		return bigInteger.doubleValue();
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return new BigDecimal(bigInteger, 1).multiply(BigDecimal.TEN);
	}

	@Override
	public RealStruct zeroValue() {
		return ZERO;
	}

	@Override
	public RealStruct max(final RealStruct real) {
		return IntegerMaxStrategy.INSTANCE.max(this, real);
	}

	@Override
	public RealStruct min(final RealStruct real) {
		return IntegerMinStrategy.INSTANCE.min(this, real);
	}

	@Override
	public RationalStruct rational() {
		return this;
	}

	@Override
	public QuotientRemainderResult floor(final RealStruct divisor) {
		return IntegerQuotientRemainderStrategy.INSTANCE.floor(this, divisor);
	}

	@Override
	public QuotientRemainderResult ffloor(final RealStruct divisor) {
		return IntegerQuotientRemainderStrategy.INSTANCE.ffloor(this, divisor);
	}

	@Override
	public QuotientRemainderResult ceiling(final RealStruct divisor) {
		return IntegerQuotientRemainderStrategy.INSTANCE.ceiling(this, divisor);
	}

	@Override
	public QuotientRemainderResult fceiling(final RealStruct divisor) {
		return IntegerQuotientRemainderStrategy.INSTANCE.fceiling(this, divisor);
	}

	@Override
	public QuotientRemainderResult round(final RealStruct divisor) {
		return IntegerQuotientRemainderStrategy.INSTANCE.round(this, divisor);
	}

	@Override
	public QuotientRemainderResult fround(final RealStruct divisor) {
		return IntegerQuotientRemainderStrategy.INSTANCE.fround(this, divisor);
	}

	@Override
	public IntegerStruct numerator() {
		return this;
	}

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
		return new IntegerStruct(bigInteger.shiftLeft(countInt));
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
		return new IntegerStruct(bigInteger.and(integer.bigInteger));
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
		return new IntegerStruct(bigInteger.not().and(integer.bigInteger));
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
		return new IntegerStruct(bigInteger.and(integer.bigInteger.not()));
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
		return new IntegerStruct(bigInteger.xor(integer.bigInteger).not());
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
		return new IntegerStruct(bigInteger.or(integer.bigInteger));
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
		return new IntegerStruct(bigInteger.and(integer.bigInteger).not());
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
		return new IntegerStruct(bigInteger.or(integer.bigInteger).not());
	}

	/**
	 * Returns the bit-wise logical 'not' of this IntegerStruct.
	 *
	 * @return the bit-wise logical 'not' of this IntegerStruct
	 */
	public IntegerStruct logNot() {
		return new IntegerStruct(bigInteger.not());
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
		return new IntegerStruct(bigInteger.not().or(integer.bigInteger));
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
		return new IntegerStruct(bigInteger.or(integer.bigInteger.not()));
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
		return new IntegerStruct(bigInteger.xor(integer.bigInteger));
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
		return bigInteger.and(integer.bigInteger).signum() != 0;
	}

	// Strategy Implementations

	/**
	 * {@link RealAddStrategy} for computing addition results for {@link IntegerStruct}s.
	 */
	private static class IntegerAddStrategy extends RealAddStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerAddStrategy} type.
		 */
		private static final IntegerAddStrategy INSTANCE = new IntegerAddStrategy();

		@Override
		public RealStruct add(final IntegerStruct number1, final IntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger add = bigInteger1.add(bigInteger2);
			return new IntegerStruct(add);
		}

		@Override
		public RealStruct add(final IntegerStruct number1, final RatioStruct number2) {
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
	 * {@link RealSubtractStrategy} for computing subtraction function results for {@link IntegerStruct}s.
	 */
	private static class IntegerSubtractStrategy extends RealSubtractStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerSubtractStrategy} type.
		 */
		private static final IntegerSubtractStrategy INSTANCE = new IntegerSubtractStrategy();

		@Override
		public RealStruct subtract(final IntegerStruct number1, final IntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger subtract = bigInteger1.subtract(bigInteger2);
			return new IntegerStruct(subtract);
		}

		@Override
		public RealStruct subtract(final IntegerStruct number1, final RatioStruct number2) {
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
	 * {@link RealMultiplyStrategy} for computing multiplication function results for {@link IntegerStruct}s.
	 */
	private static class IntegerMultiplyStrategy extends RealMultiplyStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerMultiplyStrategy} type.
		 */
		private static final IntegerMultiplyStrategy INSTANCE = new IntegerMultiplyStrategy();

		@Override
		public RealStruct multiply(final IntegerStruct number1, final IntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger multiply = bigInteger1.multiply(bigInteger2);
			return new IntegerStruct(multiply);
		}

		@Override
		public RealStruct multiply(final IntegerStruct number1, final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(numerator);
			return makeRational(multiply, denominator);
		}
	}

	/**
	 * {@link RealDivideStrategy} for computing division function results for {@link IntegerStruct}s.
	 */
	private static class IntegerDivideStrategy extends RealDivideStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerDivideStrategy} type.
		 */
		private static final IntegerDivideStrategy INSTANCE = new IntegerDivideStrategy();

		@Override
		public RealStruct divide(final IntegerStruct number1, final IntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			return makeRational(bigInteger1, bigInteger2);
		}

		@Override
		public RealStruct divide(final IntegerStruct number1, final RatioStruct number2) {
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
	 * {@link RealEqualToStrategy} for computing numeric '=' equality results for {@link IntegerStruct}s.
	 */
	private static class IntegerEqualToStrategy extends RealEqualToStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerEqualToStrategy} type.
		 */
		private static final IntegerEqualToStrategy INSTANCE = new IntegerEqualToStrategy();

		@Override
		public boolean equalTo(final IntegerStruct number1, final IntegerStruct number2) {
			final int comparisonResult = getComparisonResult(number1, number2);
			return comparisonResult == 0;
		}

		@Override
		public boolean equalTo(final IntegerStruct number1, final RatioStruct number2) {
			final int comparisonResult = getComparisonResult(number1, number2);
			return comparisonResult == 0;
		}
	}

	/**
	 * {@link LessThanStrategy} for computing numeric '<' equality results for {@link IntegerStruct}s.
	 */
	private static class IntegerLessThanStrategy extends LessThanStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerLessThanStrategy} type.
		 */
		private static final IntegerLessThanStrategy INSTANCE = new IntegerLessThanStrategy();

		@Override
		public boolean lessThan(final IntegerStruct real1, final IntegerStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		@Override
		public boolean lessThan(final IntegerStruct real1, final RatioStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}
	}

	/**
	 * {@link GreaterThanStrategy} for computing numeric '>' equality results for {@link IntegerStruct}s.
	 */
	private static class IntegerGreaterThanStrategy extends GreaterThanStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerGreaterThanStrategy} type.
		 */
		private static final IntegerGreaterThanStrategy INSTANCE = new IntegerGreaterThanStrategy();

		@Override
		public boolean greaterThan(final IntegerStruct real1, final IntegerStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		@Override
		public boolean greaterThan(final IntegerStruct real1, final RatioStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}
	}

	/**
	 * {@link LessThanOrEqualToStrategy} for computing numeric '<=' equality results for {@link IntegerStruct}s.
	 */
	private static class IntegerLessThanOrEqualToStrategy extends LessThanOrEqualToStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerLessThanOrEqualToStrategy} type.
		 */
		private static final IntegerLessThanOrEqualToStrategy INSTANCE = new IntegerLessThanOrEqualToStrategy();

		@Override
		public boolean lessThanOrEqualTo(final IntegerStruct real1, final IntegerStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final IntegerStruct real1, final RatioStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}
	}

	/**
	 * {@link GreaterThanOrEqualToStrategy} for computing numeric '>=' equality results for {@link IntegerStruct}s.
	 */
	private static class IntegerGreaterThanOrEqualToStrategy extends GreaterThanOrEqualToStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerGreaterThanOrEqualToStrategy} type.
		 */
		private static final IntegerGreaterThanOrEqualToStrategy INSTANCE = new IntegerGreaterThanOrEqualToStrategy();

		@Override
		public boolean greaterThanOrEqualTo(final IntegerStruct real1, final IntegerStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final IntegerStruct real1, final RatioStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}
	}

	/**
	 * {@link MaxStrategy} for computing the comparable maximum numerical results for {@link IntegerStruct}s.
	 */
	private static class IntegerMaxStrategy extends MaxStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerMaxStrategy} type.
		 */
		private static final IntegerMaxStrategy INSTANCE = new IntegerMaxStrategy();

		@Override
		public RealStruct max(final IntegerStruct real1, final IntegerStruct real2) {
			final BigInteger bigInteger1 = real1.getBigInteger();
			final BigInteger bigInteger2 = real2.getBigInteger();

			final BigInteger max = bigInteger1.max(bigInteger2);
			return (bigInteger1.compareTo(max) == 0) ? real1 : real2;
		}
	}

	/**
	 * {@link MinStrategy} for computing the comparable minimum numerical results for {@link IntegerStruct}s.
	 */
	private static class IntegerMinStrategy extends MinStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerMinStrategy} type.
		 */
		private static final IntegerMinStrategy INSTANCE = new IntegerMinStrategy();

		@Override
		public RealStruct min(final IntegerStruct real1, final IntegerStruct real2) {
			final BigInteger bigInteger1 = real1.getBigInteger();
			final BigInteger bigInteger2 = real2.getBigInteger();

			final BigInteger min = bigInteger1.min(bigInteger2);
			return (bigInteger1.compareTo(min) == 0) ? real1 : real2;
		}
	}

	/**
	 * {@link IntegerQuotientRemainderStrategy} for computing quotient and remainder results for {@link
	 * IntegerStruct}s.
	 */
	private static class IntegerQuotientRemainderStrategy extends RationalQuotientRemainderStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerQuotientRemainderStrategy} type.
		 */
		private static final IntegerQuotientRemainderStrategy INSTANCE = new IntegerQuotientRemainderStrategy();

		@Override
		public QuotientRemainderResult quotientRemainder(final IntegerStruct real, final IntegerStruct divisor,
		                                                 final RoundingMode roundingMode, final boolean isFloatResult) {
			final BigDecimal realBigDecimal = real.bigDecimalValue();
			final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();

			final BigDecimal quotient = realBigDecimal.divide(divisorBigDecimal, 0, roundingMode);
			final BigDecimal remainder = realBigDecimal.subtract(divisorBigDecimal.multiply(quotient));

			final RealStruct quotientReal;
			if (isFloatResult) {
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
	 * {@link RealExptStrategy} for computing exponential function results for {@link IntegerStruct}s.
	 */
	private static class IntegerExptStrategy extends RealExptStrategy<IntegerStruct> {

		/**
		 * Singleton instance of the {@link IntegerExptStrategy} type.
		 */
		private static final IntegerExptStrategy INSTANCE = new IntegerExptStrategy();

		@Override
		public NumberStruct expt(final IntegerStruct number1, final IntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger powerBigInteger = number2.getBigInteger();
			final BigInteger pow = ArithmeticUtils.pow(bigInteger1, powerBigInteger);
			return new IntegerStruct(pow);
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

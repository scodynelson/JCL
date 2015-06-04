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
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public class IntegerStruct extends RationalStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4665072618932472349L;

	public static final IntegerStruct ZERO = new IntegerStruct(BigInteger.ZERO);

	public static final IntegerStruct ONE = new IntegerStruct(BigInteger.ONE);

	public static final IntegerStruct TWO = new IntegerStruct(BigInteger.valueOf(2));

	public static final IntegerStruct MINUS_ONE = new IntegerStruct(BigInteger.valueOf(-1));

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
	public boolean eql(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean equal(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean equalp(final LispStruct lispStruct) {
		return (lispStruct instanceof NumberStruct) && isEqualTo((NumberStruct) lispStruct);
	}

	@Override
	public RealStruct abs() {
		if (bigInteger.signum() >= 0) {
			return this;
		}
		final BigInteger negate = bigInteger.negate();
		return new IntegerStruct(negate);
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

	public boolean evenp() {
		return !bigInteger.testBit(0);
	}

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
		final BigInteger negate = bigInteger.negate();
		return new IntegerStruct(negate);
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

	public IntegerStruct isqrt() {
		final double doubleValue = doubleValue();
		final double sqrt = FastMath.sqrt(doubleValue);
		final double isqrt = FastMath.floor(sqrt);
		final BigDecimal isqrtBigDecimal = BigDecimal.valueOf(isqrt);
		final BigInteger isqrtBigInteger = isqrtBigDecimal.toBigInteger();
		return new IntegerStruct(isqrtBigInteger);
	}

	@Override
	public double doubleValue() {
		return bigInteger.doubleValue();
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return new BigDecimal(bigInteger);
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

	public IntegerStruct gcd(final IntegerStruct integer) {
		final BigInteger gcd = bigInteger.gcd(integer.bigInteger);
		return new IntegerStruct(gcd);
	}

	public IntegerStruct lcm(final IntegerStruct integer) {
		if (equals(ZERO) || integer.equals(ZERO)) {
			return ZERO;
		}

		final BigInteger multiply = bigInteger.multiply(integer.bigInteger);
		final BigInteger abs = multiply.abs();
		final BigInteger gcd = bigInteger.gcd(integer.bigInteger);
		final BigInteger divide = abs.divide(gcd);

		return new IntegerStruct(divide);
	}

	public IntegerStruct ash(final IntegerStruct integer) {
		if (integer.equals(ZERO)) {
			return this;
		}

		final BigInteger count = integer.bigInteger;
		final int countInt = count.intValue();

		final BigInteger shiftedBigInteger;
		if (integer.plusp()) {
			shiftedBigInteger = bigInteger.shiftLeft(countInt);
		} else {
			shiftedBigInteger = bigInteger.shiftRight(countInt);
		}
		return new IntegerStruct(shiftedBigInteger);
	}

	public IntegerStruct logAnd(final IntegerStruct integer) {
		final BigInteger and = bigInteger.and(integer.bigInteger);
		return new IntegerStruct(and);
	}

	public IntegerStruct logAndC1(final IntegerStruct integer) {
		final BigInteger andC1 = bigInteger.not().and(integer.bigInteger);
		return new IntegerStruct(andC1);
	}

	public IntegerStruct logAndC2(final IntegerStruct integer) {
		final BigInteger andC2 = bigInteger.and(integer.bigInteger.not());
		return new IntegerStruct(andC2);
	}

	public IntegerStruct logEqv(final IntegerStruct integer) {
		final BigInteger eqv = bigInteger.xor(integer.bigInteger).not();
		return new IntegerStruct(eqv);
	}

	public IntegerStruct logIor(final IntegerStruct integer) {
		final BigInteger ior = bigInteger.or(integer.bigInteger);
		return new IntegerStruct(ior);
	}

	public IntegerStruct logNand(final IntegerStruct integer) {
		final BigInteger nand = bigInteger.and(integer.bigInteger).not();
		return new IntegerStruct(nand);
	}

	public IntegerStruct logNor(final IntegerStruct integer) {
		final BigInteger nor = bigInteger.or(integer.bigInteger).not();
		return new IntegerStruct(nor);
	}

	public IntegerStruct logNot() {
		final BigInteger not = bigInteger.not();
		return new IntegerStruct(not);
	}

	public IntegerStruct logOrC1(final IntegerStruct integer) {
		final BigInteger orC1 = bigInteger.not().or(integer.bigInteger);
		return new IntegerStruct(orC1);
	}

	public IntegerStruct logOrC2(final IntegerStruct integer) {
		final BigInteger orC2 = bigInteger.or(integer.bigInteger.not());
		return new IntegerStruct(orC2);
	}

	public IntegerStruct logXor(final IntegerStruct integer) {
		final BigInteger xor = bigInteger.xor(integer.bigInteger);
		return new IntegerStruct(xor);
	}

	public IntegerStruct integerLength() {
		final int bitLength = bigInteger.bitLength();
		final BigInteger bitLengthBigInteger = BigInteger.valueOf(bitLength);
		return new IntegerStruct(bitLengthBigInteger);
	}

	public boolean logBitP(final IntegerStruct index) {
		final BigInteger indexBigInteger = index.bigInteger;
		final int indexInt = indexBigInteger.intValue();
		return bigInteger.testBit(indexInt);
	}

	public IntegerStruct logCount() {
		final int bitCount = bigInteger.bitCount();
		final BigInteger bitCountBigInteger = BigInteger.valueOf(bitCount);
		return new IntegerStruct(bitCountBigInteger);
	}

	public boolean logTest(final IntegerStruct integer) {
		return bigInteger.and(integer.bigInteger).signum() == 0;
	}

	// Strategy Implementations

	private static class IntegerAddStrategy extends RealAddStrategy<IntegerStruct> {

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

	private static class IntegerSubtractStrategy extends RealSubtractStrategy<IntegerStruct> {

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

	private static class IntegerMultiplyStrategy extends RealMultiplyStrategy<IntegerStruct> {

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

	private static class IntegerDivideStrategy extends RealDivideStrategy<IntegerStruct> {

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

	private static class IntegerEqualToStrategy extends EqualToStrategy<IntegerStruct> {

		private static final IntegerEqualToStrategy INSTANCE = new IntegerEqualToStrategy();

		@Override
		public boolean equalTo(final IntegerStruct number1, final IntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			return bigInteger1.equals(bigInteger2);
		}

		@Override
		public boolean equalTo(final IntegerStruct number1, final FloatStruct number2) {
			final RationalStruct rational = number2.rational();
			return INSTANCE.equalTo(number1, rational);
		}

		@Override
		public boolean equalTo(final IntegerStruct number1, final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigFraction bigFraction2Reduced = bigFraction2.reduce();
			final BigInteger numerator = bigFraction2Reduced.getNumerator();
			final BigInteger denominator = bigFraction2Reduced.getDenominator();

			return denominator.equals(BigInteger.ONE) && bigInteger1.equals(numerator);
		}

		@Override
		public boolean equalTo(final IntegerStruct number1, final ComplexStruct number2) {
			return false;
		}
	}

	private static class IntegerLessThanStrategy extends LessThanStrategy<IntegerStruct> {

		private static final IntegerLessThanStrategy INSTANCE = new IntegerLessThanStrategy();

		@Override
		public boolean lessThan(final IntegerStruct real1, final IntegerStruct real2) {
			final BigInteger bigInteger1 = real1.getBigInteger();
			final BigInteger bigInteger2 = real2.getBigInteger();
			return bigInteger1.compareTo(bigInteger2) < 0;
		}

		@Override
		public boolean lessThan(final IntegerStruct real1, final FloatStruct real2) {
			final RationalStruct rational = real2.rational();
			return INSTANCE.lessThan(real1, rational);
		}

		@Override
		public boolean lessThan(final IntegerStruct real1, final RatioStruct real2) {
			final BigInteger bigInteger1 = real1.getBigInteger();

			final BigFraction bigFraction2 = real2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			return multiply.compareTo(numerator) < 0;
		}
	}

	private static class IntegerGreaterThanStrategy extends GreaterThanStrategy<IntegerStruct> {

		private static final IntegerGreaterThanStrategy INSTANCE = new IntegerGreaterThanStrategy();

		@Override
		public boolean greaterThan(final IntegerStruct real1, final IntegerStruct real2) {
			final BigInteger bigInteger1 = real1.getBigInteger();
			final BigInteger bigInteger2 = real2.getBigInteger();
			return bigInteger1.compareTo(bigInteger2) > 0;
		}

		@Override
		public boolean greaterThan(final IntegerStruct real1, final FloatStruct real2) {
			final RationalStruct rational = real2.rational();
			return INSTANCE.greaterThan(real1, rational);
		}

		@Override
		public boolean greaterThan(final IntegerStruct real1, final RatioStruct real2) {
			final BigInteger bigInteger1 = real1.getBigInteger();

			final BigFraction bigFraction2 = real2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			return multiply.compareTo(numerator) > 0;
		}
	}

	private static class IntegerLessThanOrEqualToStrategy extends LessThanOrEqualToStrategy<IntegerStruct> {

		private static final IntegerLessThanOrEqualToStrategy INSTANCE = new IntegerLessThanOrEqualToStrategy();

		@Override
		public boolean lessThanOrEqualTo(final IntegerStruct real1, final IntegerStruct real2) {
			final BigInteger bigInteger1 = real1.getBigInteger();
			final BigInteger bigInteger2 = real2.getBigInteger();
			return bigInteger1.compareTo(bigInteger2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final IntegerStruct real1, final FloatStruct real2) {
			final RationalStruct rational = real2.rational();
			return INSTANCE.lessThanOrEqualTo(real1, rational);
		}

		@Override
		public boolean lessThanOrEqualTo(final IntegerStruct real1, final RatioStruct real2) {
			final BigInteger bigInteger1 = real1.getBigInteger();

			final BigFraction bigFraction2 = real2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			return multiply.compareTo(numerator) <= 0;
		}
	}

	private static class IntegerGreaterThanOrEqualToStrategy extends GreaterThanOrEqualToStrategy<IntegerStruct> {

		private static final IntegerGreaterThanOrEqualToStrategy INSTANCE = new IntegerGreaterThanOrEqualToStrategy();

		@Override
		public boolean greaterThanOrEqualTo(final IntegerStruct real1, final IntegerStruct real2) {
			final BigInteger bigInteger2 = real2.getBigInteger();
			final BigInteger bigInteger1 = real1.getBigInteger();
			return bigInteger1.compareTo(bigInteger2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final IntegerStruct real1, final FloatStruct real2) {
			final RationalStruct rational = real2.rational();
			return INSTANCE.greaterThanOrEqualTo(real1, rational);
		}

		@Override
		public boolean greaterThanOrEqualTo(final IntegerStruct real1, final RatioStruct real2) {
			final BigInteger bigInteger1 = real1.getBigInteger();

			final BigFraction bigFraction2 = real2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			return multiply.compareTo(numerator) >= 0;
		}
	}

	private static class IntegerMaxStrategy extends MaxStrategy<IntegerStruct> {

		private static final IntegerMaxStrategy INSTANCE = new IntegerMaxStrategy();

		@Override
		public RealStruct max(final IntegerStruct real1, final IntegerStruct real2) {
			final BigInteger bigInteger1 = real1.getBigInteger();
			final BigInteger bigInteger2 = real2.getBigInteger();

			final BigInteger max = bigInteger1.max(bigInteger2);
			return Objects.equals(bigInteger1, max) ? real1 : real2;
		}
	}

	private static class IntegerMinStrategy extends MinStrategy<IntegerStruct> {

		private static final IntegerMinStrategy INSTANCE = new IntegerMinStrategy();

		@Override
		public RealStruct min(final IntegerStruct real1, final IntegerStruct real2) {
			final BigInteger bigInteger1 = real1.getBigInteger();
			final BigInteger bigInteger2 = real2.getBigInteger();

			final BigInteger min = bigInteger1.min(bigInteger2);
			return Objects.equals(bigInteger1, min) ? real1 : real2;
		}
	}

	private static class IntegerQuotientRemainderStrategy extends RationalQuotientRemainderStrategy<IntegerStruct> {

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

	private static class IntegerExptStrategy extends RealExptStrategy<IntegerStruct> {

		private static final IntegerExptStrategy INSTANCE = new IntegerExptStrategy();

		@Override
		public NumberStruct expt(final IntegerStruct number1, final IntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger powerBigInteger = number2.getBigInteger();
			final BigInteger pow = ArithmeticUtils.pow(bigInteger1, powerBigInteger);
			return new IntegerStruct(pow);
		}
	}

	// Static Multi-Arg Methods

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

	// HashCode / Equals / ToString

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

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bigInteger)
		                                                                .toString();
	}
}

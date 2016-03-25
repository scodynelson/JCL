/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigInteger;

import jcl.classes.BuiltInClassStruct;
import jcl.types.BignumType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link IntIntegerStruct} is the object representation of a Lisp 'integer' type that can be contained within a
 * Java {@link BigInteger}.
 */
public final class BigIntegerStruct extends BuiltInClassStruct implements IntegerStruct {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(BigIntegerStruct.class);

	/**
	 * The internal {@link BigInteger} containing the BigIntegerStruct contents.
	 */
	final BigInteger bigInteger;

	/**
	 * Private constructor.
	 *
	 * @param bigInteger
	 * 		the value of the BigIntegerStruct
	 */
	private BigIntegerStruct(final BigInteger bigInteger) {
		super(BignumType.INSTANCE, null, null);
		this.bigInteger = bigInteger;
	}

	/**
	 * Returns a BigIntegerStruct object with the provided {@link BigInteger} value.
	 *
	 * @param bigInteger
	 * 		the {@link BigInteger} value of the resulting BigIntegerStruct
	 *
	 * @return a BigIntegerStruct object with the provided {@link BigInteger} value
	 */
	public static BigIntegerStruct valueOf(final BigInteger bigInteger) {
		return valueOf(bigInteger);
	}

	/*
		IntegerStruct
	 */

	@Override
	public int intValue() {
		try {
			return bigInteger.intValueExact();
		} catch (final ArithmeticException ignore) {
			LOGGER.warn("Loss of precision when converting BigInteger to int.");
			return bigInteger.intValue();
		}
	}

	@Override
	public long longValue() {
		try {
			return bigInteger.longValueExact();
		} catch (final ArithmeticException ignore) {
			LOGGER.warn("Loss of precision when converting BigInteger to long.");
			return bigInteger.longValue();
		}
	}

	@Override
	public BigInteger bigIntegerValue() {
		return bigInteger;
	}

	@Override
	public IntegerStruct gcd(final IntegerStruct.GcdVisitor<?> gcdVisitor) {
		return gcdVisitor.gcd(this);
	}

	@Override
	public IntegerStruct.GcdVisitor<?> gcdVisitor() {
		return new BigIntegerGcdVisitor(this);
	}

	@Override
	public IntegerStruct lcm(final IntegerStruct.LcmVisitor<?> lcmVisitor) {
		return lcmVisitor.lcm(this);
	}

	@Override
	public IntegerStruct.LcmVisitor<?> lcmVisitor() {
		return new BigIntegerLcmVisitor(this);
	}

	@Override
	public IntegerStruct ash(final AshVisitor<?> ashVisitor) {
		return ashVisitor.ash(this);
	}

	@Override
	public AshVisitor<?> ashVisitor() {
		return new BigIntegerAshVisitor(this);
	}

	@Override
	public IntegerStruct logAnd(final LogAndVisitor<?> logAndVisitor) {
		return logAndVisitor.logAnd(this);
	}

	@Override
	public LogAndVisitor<?> logAndVisitor() {
		return new BigIntegerLogAndVisitor(this);
	}

	@Override
	public IntegerStruct logAndC1(final LogAndC1Visitor<?> logAndC1Visitor) {
		return logAndC1Visitor.logAndC1(this);
	}

	@Override
	public LogAndC1Visitor<?> logAndC1Visitor() {
		return new BigIntegerLogAndC1Visitor(this);
	}

	@Override
	public IntegerStruct logAndC2(final LogAndC2Visitor<?> logAndC2Visitor) {
		return logAndC2Visitor.logAndC2(this);
	}

	@Override
	public LogAndC2Visitor<?> logAndC2Visitor() {
		return new BigIntegerLogAndC2Visitor(this);
	}

	@Override
	public IntegerStruct logEqv(final LogEqvVisitor<?> logEqvVisitor) {
		return logEqvVisitor.logEqv(this);
	}

	@Override
	public LogEqvVisitor<?> logEqvVisitor() {
		return new BigIntegerLogEqvVisitor(this);
	}

	@Override
	public IntegerStruct logIor(final LogIorVisitor<?> logIorVisitor) {
		return logIorVisitor.logIor(this);
	}

	@Override
	public LogIorVisitor<?> logIorVisitor() {
		return new BigIntegerLogIorVisitor(this);
	}

	@Override
	public IntegerStruct logNand(final LogNandVisitor<?> logNandVisitor) {
		return logNandVisitor.logNand(this);
	}

	@Override
	public LogNandVisitor<?> logNandVisitor() {
		return new BigIntegerLogNandVisitor(this);
	}

	@Override
	public IntegerStruct logNor(final LogNorVisitor<?> logNorVisitor) {
		return logNorVisitor.logNor(this);
	}

	@Override
	public LogNorVisitor<?> logNorVisitor() {
		return new BigIntegerLogNorVisitor(this);
	}

	@Override
	public BigIntegerStruct logNot() {
		return valueOf(bigInteger.not());
	}

	@Override
	public IntegerStruct logOrC1(final LogOrC1Visitor<?> logOrC1Visitor) {
		return logOrC1Visitor.logOrC1(this);
	}

	@Override
	public LogOrC1Visitor<?> logOrC1Visitor() {
		return new BigIntegerLogOrC1Visitor(this);
	}

	@Override
	public IntegerStruct logOrC2(final LogOrC2Visitor<?> logOrC2Visitor) {
		return logOrC2Visitor.logOrC2(this);
	}

	@Override
	public LogOrC2Visitor<?> logOrC2Visitor() {
		return new BigIntegerLogOrC2Visitor(this);
	}

	@Override
	public IntegerStruct logXor(final LogXorVisitor<?> logXorVisitor) {
		return logXorVisitor.logXor(this);
	}

	@Override
	public LogXorVisitor<?> logXorVisitor() {
		return new BigIntegerLogXorVisitor(this);
	}

	@Override
	public boolean logBitP(final LogBitPVisitor<?> logBitPVisitor) {
		return logBitPVisitor.logBitP(this);
	}

	@Override
	public LogBitPVisitor<?> logBitPVisitor() {
		return new BigIntegerLogBitPVisitor(this);
	}

	@Override
	public IntegerStruct logCount() {
		final int bitCount = bigInteger.bitCount();
		return IntegerStruct.valueOf(bitCount);
	}

	@Override
	public IntegerStruct integerLength() {
		final int bitLength = bigInteger.bitLength();
		return IntegerStruct.valueOf(bitLength);
	}

	@Override
	public boolean evenp() {
		return !bigInteger.testBit(0);
	}

	@Override
	public boolean oddp() {
		return bigInteger.testBit(0);
	}

	@Override
	public BigIntegerStruct isqrt() {
		// TODO
		final Apfloat apfloat = apfloatValue();
		final Apfloat sqrt = ApfloatMath.sqrt(apfloat);
		final Apint floor = sqrt.floor();
		final BigInteger floorBigInteger = floor.toBigInteger();
		return valueOf(floorBigInteger);
	}

	/*
		RealStruct
	 */

	@Override
	public FloatStruct coerceRealToFloat() {
		// TODO
		return FloatStruct.valueOf(bigDecimalValue());
	}

	@Override
	public boolean isLessThan(final RealStruct.LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	@Override
	public RealStruct.LessThanVisitor<?> lessThanVisitor() {
		return new BigIntegerLessThanVisitor(this);
	}

	@Override
	public boolean isGreaterThan(final RealStruct.GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	@Override
	public RealStruct.GreaterThanVisitor<?> greaterThanVisitor() {
		return new BigIntegerGreaterThanVisitor(this);
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	@Override
	public RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new BigIntegerLessThanOrEqualToVisitor(this);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	@Override
	public RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new BigIntegerGreaterThanOrEqualToVisitor(this);
	}

	@Override
	public boolean plusp() {
		return bigInteger.signum() > 0;
	}

	@Override
	public boolean minusp() {
		return bigInteger.signum() < 0;
	}

	/*
		NumberStruct
	 */

	@Override
	public NumberStruct add(final AddVisitor<?> addVisitor) {
		return addVisitor.add(this);
	}

	@Override
	public AddVisitor<?> addVisitor() {
		return new BigIntegerAddVisitor(this);
	}

	@Override
	public NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	public SubtractVisitor<?> subtractVisitor() {
		return new BigIntegerSubtractVisitor(this);
	}

	@Override
	public NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	public MultiplyVisitor<?> multiplyVisitor() {
		return new BigIntegerMultiplyVisitor(this);
	}

	@Override
	public NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	public DivideVisitor<?> divideVisitor() {
		return new BigIntegerDivideVisitor(this);
	}

	@Override
	public boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	@Override
	public EqualToVisitor<?> equalToVisitor() {
		return new BigIntegerEqualToVisitor(this);
	}

	@Override
	public NumberStruct expt(final ExptVisitor<?> exptVisitor) {
		return exptVisitor.expt(this);
	}

	@Override
	public ExptVisitor<?> exptVisitor() {
		return new BigIntegerExptVisitor(this);
	}

	@Override
	public boolean zerop() {
		return bigInteger.signum() == 0;
	}

	@Override
	public RealStruct abs() {
		if (bigInteger.signum() >= 0) {
			return this;
		}
		return negation();
	}

	@Override
	public IntegerStruct negation() {
		return valueOf(bigInteger.negate());
	}

	@Override
	public NumberStruct reciprocal() {
		if (BigInteger.ONE.equals(bigInteger)) {
			return this;
		}
		return new RatioStruct(BigInteger.ONE, bigInteger);
	}

	private static int getComparisonResult(final BigIntegerStruct number1, final BigIntegerStruct number2) {
		final BigInteger bigInteger1 = number1.bigInteger;
		final BigInteger bigInteger2 = number2.bigInteger;
		return bigInteger1.compareTo(bigInteger2);
	}

	private static int getComparisonResult(final BigIntegerStruct number1, final RatioStruct number2) {
		final BigInteger bigInteger1 = number1.bigInteger;

		final BigFraction bigFraction2 = number2.getBigFraction();
		final BigFraction bigFraction2Reduced = bigFraction2.reduce();
		final BigInteger numerator = bigFraction2Reduced.getNumerator();
		final BigInteger denominator = bigFraction2Reduced.getDenominator();

		final BigInteger multiply = bigInteger1.multiply(denominator);
		return multiply.compareTo(numerator);
	}

	// HashCode / Equals

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(bigInteger)
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
		final BigIntegerStruct rhs = (BigIntegerStruct) obj;
		return new EqualsBuilder().append(bigInteger, rhs.bigInteger)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return bigInteger.toString();
	}

	// Visitor Implementations

	/**
	 * {@link RealStruct.RealAddVisitor} for computing addition results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerAddVisitor extends RealStruct.RealAddVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerAddVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		private BigIntegerAddVisitor(final BigIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct add(final IntIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger add = bigInteger1.add(bigInteger2);
			return valueOf(add);
		}

		@Override
		public RealStruct add(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			final BigInteger add = multiply.add(numerator);
			return RationalStruct.makeRational(add, denominator);
		}
	}

	/**
	 * {@link RealStruct.RealSubtractVisitor} for computing subtraction function results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerSubtractVisitor extends RealStruct.RealSubtractVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerSubtractVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		private BigIntegerSubtractVisitor(final BigIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct subtract(final IntIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger subtract = bigInteger1.subtract(bigInteger2);
			return valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			final BigInteger subtract = multiply.subtract(numerator);
			return RationalStruct.makeRational(subtract, denominator);
		}
	}

	/**
	 * {@link RealStruct.RealMultiplyVisitor} for computing multiplication function results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerMultiplyVisitor extends RealStruct.RealMultiplyVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerMultiplyVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		private BigIntegerMultiplyVisitor(final BigIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct multiply(final IntIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger multiply = bigInteger1.multiply(bigInteger2);
			return valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(numerator);
			return RationalStruct.makeRational(multiply, denominator);
		}
	}

	/**
	 * {@link RealStruct.RealDivideVisitor} for computing division function results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerDivideVisitor extends RealStruct.RealDivideVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerDivideVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		private BigIntegerDivideVisitor(final BigIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct divide(final IntIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			return RationalStruct.makeRational(bigInteger1, bigInteger2);
		}

		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			return RationalStruct.makeRational(multiply, numerator);
		}
	}

	/**
	 * {@link RealStruct.RealEqualToVisitor} for computing numeric '=' equality results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerEqualToVisitor extends RealStruct.RealEqualToVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerEqualToVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		private BigIntegerEqualToVisitor(final BigIntegerStruct number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final BigIntegerStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		@Override
		public boolean equalTo(final RatioStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}
	}

	/**
	 * {@link RealStruct.LessThanVisitor} for computing numeric {@literal '<'} equality results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerLessThanVisitor extends RealStruct.LessThanVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerLessThanVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		private BigIntegerLessThanVisitor(final BigIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThan(final BigIntegerStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		@Override
		public boolean lessThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}
	}

	/**
	 * {@link RealStruct.GreaterThanVisitor} for computing numeric {@literal '>'} equality results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerGreaterThanVisitor extends RealStruct.GreaterThanVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerGreaterThanVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		private BigIntegerGreaterThanVisitor(final BigIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThan(final BigIntegerStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		@Override
		public boolean greaterThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}
	}

	/**
	 * {@link RealStruct.LessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerLessThanOrEqualToVisitor extends RealStruct.LessThanOrEqualToVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerLessThanOrEqualToVisitor with the provided
		 * {@link BigIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		private BigIntegerLessThanOrEqualToVisitor(final BigIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThanOrEqualTo(final BigIntegerStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}
	}

	/**
	 * {@link RealStruct.GreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerGreaterThanOrEqualToVisitor extends RealStruct.GreaterThanOrEqualToVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerGreaterThanOrEqualToVisitor with the
		 * provided {@link BigIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		private BigIntegerGreaterThanOrEqualToVisitor(final BigIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigIntegerStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}
	}

	/**
	 * {@link RealStruct.RealExptVisitor} for computing exponential function results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerExptVisitor extends RealStruct.RealExptVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntegerExptVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param base
		 * 		the base argument in the exponential operation
		 */
		private BigIntegerExptVisitor(final BigIntegerStruct base) {
			super(base);
		}

		@Override
		public NumberStruct expt(final BigIntegerStruct power) {
			if (power.minusp()) {
				return exptInteger(base, power);
			} else {
				final BigInteger baseBigInteger = base.getBigInteger();
				final BigInteger powerBigInteger = power.getBigInteger();
				final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, powerBigInteger);
				return valueOf(pow);
			}
		}
	}

	private static final class BigIntegerGcdVisitor extends IntegerStruct.GcdVisitor<BigIntegerStruct> {

		private BigIntegerGcdVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct gcd(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct gcd(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct gcd(final BigIntegerStruct integer2) {
			return null;
		}
	}

	private static final class BigIntegerLcmVisitor extends IntegerStruct.LcmVisitor<BigIntegerStruct> {

		private BigIntegerLcmVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct lcm(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct lcm(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct lcm(final BigIntegerStruct integer2) {
			return null;
		}
	}

	private static final class BigIntegerAshVisitor extends IntegerStruct.AshVisitor<BigIntegerStruct> {

		private BigIntegerAshVisitor(final BigIntegerStruct integer) {
			super(integer);
		}

		@Override
		public IntegerStruct ash(final IntIntegerStruct count) {
			return null;
		}

		@Override
		public IntegerStruct ash(final LongIntegerStruct count) {
			return null;
		}

		@Override
		public IntegerStruct ash(final BigIntegerStruct count) {
//			if (count.zerop()) {
//				return this;
//			}
//
//			final int countInt = count.getBigInteger().intValue();
//
//			// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
//			final BigInteger shiftLeft = bigInteger.shiftLeft(countInt);
//			return new LongIntegerStruct(shiftLeft);
			return null;
		}
	}

	private static final class BigIntegerLogAndVisitor extends IntegerStruct.LogAndVisitor<BigIntegerStruct> {

		private BigIntegerLogAndVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAnd(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logAnd(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logAnd(final BigIntegerStruct integer2) {
//			final BigInteger and = bigInteger.and(integer.getBigInteger());
//			return valueOf(and);
			return null;
		}
	}

	private static final class BigIntegerLogAndC1Visitor extends IntegerStruct.LogAndC1Visitor<BigIntegerStruct> {

		private BigIntegerLogAndC1Visitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAndC1(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logAndC1(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logAndC1(final BigIntegerStruct integer2) {
//			final BigInteger not = bigInteger.not();
//			final BigInteger and = not.and(integer.getBigInteger());
//			return valueOf(and);
			return null;
		}
	}

	private static final class BigIntegerLogAndC2Visitor extends IntegerStruct.LogAndC2Visitor<BigIntegerStruct> {

		private BigIntegerLogAndC2Visitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAndC2(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logAndC2(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logAndC2(final BigIntegerStruct integer2) {
//			final BigInteger not = integer.getBigInteger().not();
//			final BigInteger and = bigInteger.and(not);
//			return valueOf(and);
			return null;
		}
	}

	private static final class BigIntegerLogEqvVisitor extends IntegerStruct.LogEqvVisitor<BigIntegerStruct> {

		private BigIntegerLogEqvVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logEqv(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logEqv(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logEqv(final BigIntegerStruct integer2) {
//			final BigInteger xor = bigInteger.xor(integer.getBigInteger());
//			final BigInteger not = xor.not();
//			return valueOf(not);
			return null;
		}
	}

	private static final class BigIntegerLogIorVisitor extends IntegerStruct.LogIorVisitor<BigIntegerStruct> {

		private BigIntegerLogIorVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logIor(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logIor(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logIor(final BigIntegerStruct integer2) {
			return null;
		}
	}

	private static final class BigIntegerLogNandVisitor extends IntegerStruct.LogNandVisitor<BigIntegerStruct> {

		private BigIntegerLogNandVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logNand(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logNand(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logNand(final BigIntegerStruct integer2) {
			return null;
		}
	}

	private static final class BigIntegerLogNorVisitor extends IntegerStruct.LogNorVisitor<BigIntegerStruct> {

		private BigIntegerLogNorVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logNor(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logNor(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logNor(final BigIntegerStruct integer2) {
			return null;
		}
	}

	private static final class BigIntegerLogOrC1Visitor extends IntegerStruct.LogOrC1Visitor<BigIntegerStruct> {

		private BigIntegerLogOrC1Visitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logOrC1(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logOrC1(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logOrC1(final BigIntegerStruct integer2) {
			return null;
		}
	}

	private static final class BigIntegerLogOrC2Visitor extends IntegerStruct.LogOrC2Visitor<BigIntegerStruct> {

		private BigIntegerLogOrC2Visitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logOrC2(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logOrC2(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logOrC2(final BigIntegerStruct integer2) {
			return null;
		}
	}

	private static final class BigIntegerLogXorVisitor extends IntegerStruct.LogXorVisitor<BigIntegerStruct> {

		private BigIntegerLogXorVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logXor(final IntIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logXor(final LongIntegerStruct integer2) {
			return null;
		}

		@Override
		public IntegerStruct logXor(final BigIntegerStruct integer2) {
			return null;
		}
	}

	private static final class BigIntegerLogBitPVisitor extends IntegerStruct.LogBitPVisitor<BigIntegerStruct> {

		private BigIntegerLogBitPVisitor(final BigIntegerStruct integer) {
			super(integer);
		}

		@Override
		public boolean logBitP(final IntIntegerStruct index) {
			return false;
		}

		@Override
		public boolean logBitP(final LongIntegerStruct index) {
			return false;
		}

		@Override
		public boolean logBitP(final BigIntegerStruct index) {
//			final int indexInt = index.getBigInteger().intValue();
//			return bigInteger.testBit(indexInt);
			return false;
		}
	}
}

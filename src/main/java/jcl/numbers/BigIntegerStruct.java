/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

import com.google.common.math.BigIntegerMath;
import jcl.classes.BuiltInClassStruct;
import jcl.conditions.exceptions.DivisionByZeroException;
import jcl.types.BignumType;
import jcl.util.NumberUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
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
		return new BigIntegerStruct(bigInteger);
	}

	/*
		IntegerStruct
	 */

	@Override
	public int intValue() {
		try {
			return bigInteger.intValueExact();
		} catch (final ArithmeticException ignore) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Loss of precision when converting BigInteger to int.");
			}
			return bigInteger.intValue();
		}
	}

	@Override
	public long longValue() {
		try {
			return bigInteger.longValueExact();
		} catch (final ArithmeticException ignore) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Loss of precision when converting BigInteger to long.");
			}
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
	public IntegerStruct isqrt() {
		final BigInteger sqrtFloor = BigIntegerMath.sqrt(bigInteger, RoundingMode.FLOOR);
		return IntegerStruct.valueOf(sqrtFloor);
	}

	/*
		RealStruct
	 */

	@Override
	public FloatStruct coerceRealToFloat() {
		final BigDecimal bigDecimal = NumberUtils.bigDecimalValue(bigIntegerValue());
		return FloatStruct.valueOf(bigDecimal);
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
		if (BigInteger.ZERO.equals(bigInteger)) {
			throw new DivisionByZeroException("Division by zero.");
		}
		if (BigInteger.ONE.equals(bigInteger)) {
			return this;
		}
		return RatioStruct.valueOf(BigInteger.ONE, bigInteger);
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
		 * Private constructor to make a new instance of an BigIntegerAddVisitor with the provided {@link
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
			final BigInteger bBigInteger = number1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(number2.i);
			final BigInteger add = bBigInteger.add(iBigInteger);
			return valueOf(add);
		}

		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			final BigInteger bBigInteger = number1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(number2.l);
			final BigInteger add = bBigInteger.add(lBigInteger);
			return valueOf(add);
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			final BigInteger bBigInteger1 = number1.bigInteger;
			final BigInteger bBigInteger2 = number2.bigInteger;
			final BigInteger add = bBigInteger1.add(bBigInteger2);
			return valueOf(add);
		}

		@Override
		public RealStruct add(final SingleFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			final BigDecimal add = bBigDecimal.add(bigDecimal);
			return FloatStruct.valueOf(add);
		}

		@Override
		public RealStruct add(final DoubleFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			final BigDecimal add = bBigDecimal.add(bigDecimal);
			return FloatStruct.valueOf(add);
		}

		@Override
		public RealStruct add(final BigFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal add = bBigDecimal.add(bigDecimal);
			return BigFloatStruct.valueOf(add);
		}

		@Override
		public RealStruct add(final RatioStruct number2) {
			final BigInteger bBigInteger = number1.bigInteger;
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction add = bigFraction.add(bBigInteger);
			return RationalStruct.makeRational(add);
		}

		@Override
		public NumberStruct add(final ComplexStruct number2) {
			// TODO
			return super.add(number2);
		}
	}

	/**
	 * {@link RealStruct.RealSubtractVisitor} for computing subtraction function results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerSubtractVisitor extends RealStruct.RealSubtractVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerSubtractVisitor with the provided {@link
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
			final BigInteger bBigInteger = number1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(number2.i);
			final BigInteger subtract = bBigInteger.subtract(iBigInteger);
			return IntegerStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			final BigInteger bBigInteger = number1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(number2.l);
			final BigInteger subtract = bBigInteger.subtract(lBigInteger);
			return IntegerStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			final BigInteger bBigInteger1 = number1.bigInteger;
			final BigInteger bBigInteger2 = number2.bigInteger;
			final BigInteger subtract = bBigInteger1.subtract(bBigInteger2);
			return IntegerStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final SingleFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			final BigDecimal subtract = bBigDecimal.subtract(bigDecimal);
			return FloatStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final DoubleFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			final BigDecimal subtract = bBigDecimal.subtract(bigDecimal);
			return FloatStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final BigFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal subtract = bBigDecimal.subtract(bigDecimal);
			return FloatStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final BigFraction bBigFraction = new BigFraction(number1.bigInteger);
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction subtract = bBigFraction.subtract(bigFraction);
			return RationalStruct.makeRational(subtract);
		}

		@Override
		public NumberStruct subtract(final ComplexStruct number2) {
			// TODO
			return super.subtract(number2);
		}
	}

	/**
	 * {@link RealStruct.RealMultiplyVisitor} for computing multiplication function results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerMultiplyVisitor extends RealStruct.RealMultiplyVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerMultiplyVisitor with the provided {@link
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
			final BigInteger bBigInteger = number1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(number2.i);
			final BigInteger multiply = bBigInteger.multiply(iBigInteger);
			return valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			final BigInteger bBigInteger = number1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(number2.l);
			final BigInteger multiply = bBigInteger.multiply(lBigInteger);
			return valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			final BigInteger bBigInteger1 = number1.bigInteger;
			final BigInteger bBigInteger2 = number2.bigInteger;
			final BigInteger multiply = bBigInteger1.multiply(bBigInteger2);
			return valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final SingleFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			final BigDecimal multiply = bBigDecimal.multiply(bigDecimal);
			return FloatStruct.valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final DoubleFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			final BigDecimal multiply = bBigDecimal.multiply(bigDecimal);
			return FloatStruct.valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final BigFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal multiply = bBigDecimal.multiply(bigDecimal);
			return BigFloatStruct.valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final BigInteger bBigInteger = number1.bigInteger;
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction multiply = bigFraction.multiply(bBigInteger);
			return RationalStruct.makeRational(multiply);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number2) {
			// TODO
			return super.multiply(number2);
		}
	}

	/**
	 * {@link RealStruct.RealDivideVisitor} for computing division function results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerDivideVisitor extends RealStruct.RealDivideVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerDivideVisitor with the provided {@link
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
			final BigInteger bBigInteger = number1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(number2.i);
			return RationalStruct.makeRational(bBigInteger, iBigInteger);
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			final BigInteger bBigInteger = number1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(number2.l);
			return RationalStruct.makeRational(bBigInteger, lBigInteger);
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			final BigInteger bBigInteger1 = number1.bigInteger;
			final BigInteger bBigInteger2 = number2.bigInteger;
			return RationalStruct.makeRational(bBigInteger1, bBigInteger2);
		}

		@Override
		public RealStruct divide(final SingleFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			final BigDecimal divide = bBigDecimal.divide(bigDecimal, MathContext.DECIMAL32);
			return FloatStruct.valueOf(divide);
		}

		@Override
		public RealStruct divide(final DoubleFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			final BigDecimal divide = bBigDecimal.divide(bigDecimal, MathContext.DECIMAL64);
			return FloatStruct.valueOf(divide);
		}

		@Override
		public RealStruct divide(final BigFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal divide = bBigDecimal.divide(bigDecimal, MathContext.DECIMAL128);
			return FloatStruct.valueOf(divide);
		}

		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigFraction bBigFraction = new BigFraction(number1.bigInteger);
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction divide = bBigFraction.divide(bigFraction);
			return RationalStruct.makeRational(divide);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			// TODO
			return super.divide(number2);
		}
	}

	/**
	 * {@link RealStruct.RealEqualToVisitor} for computing numeric '=' equality results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerEqualToVisitor extends RealStruct.RealEqualToVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerEqualToVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		private BigIntegerEqualToVisitor(final BigIntegerStruct number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final IntIntegerStruct number2) {
			final BigInteger bBigInteger = number1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(number2.i);
			return bBigInteger.compareTo(iBigInteger) == 0;
		}

		@Override
		public boolean equalTo(final LongIntegerStruct number2) {
			final BigInteger bBigInteger = number1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(number2.l);
			return bBigInteger.compareTo(lBigInteger) == 0;
		}

		@Override
		public boolean equalTo(final BigIntegerStruct number2) {
			final BigInteger bBigInteger1 = number1.bigInteger;
			final BigInteger bBigInteger2 = number2.bigInteger;
			return bBigInteger1.compareTo(bBigInteger2) == 0;
		}

		@Override
		public boolean equalTo(final SingleFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) == 0;
		}

		@Override
		public boolean equalTo(final DoubleFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) == 0;
		}

		@Override
		public boolean equalTo(final BigFloatStruct number2) {
			final BigDecimal bBigDecimal = new BigDecimal(number1.bigInteger);
			final BigDecimal bigDecimal = number2.bigDecimal;
			return bBigDecimal.compareTo(bigDecimal) == 0;
		}

		@Override
		public boolean equalTo(final RatioStruct number2) {
			final BigFraction bBigFraction = new BigFraction(number1.bigInteger);
			final BigFraction bigFraction = number2.bigFraction;
			return bBigFraction.equals(bigFraction);
		}
	}

	/**
	 * {@link RealStruct.LessThanVisitor} for computing numeric {@literal '<'} equality results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerLessThanVisitor extends RealStruct.LessThanVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLessThanVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		private BigIntegerLessThanVisitor(final BigIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThan(final IntIntegerStruct real2) {
			final BigInteger bBigInteger = real1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(real2.i);
			return bBigInteger.compareTo(iBigInteger) < 0;
		}

		@Override
		public boolean lessThan(final LongIntegerStruct real2) {
			final BigInteger bBigInteger = real1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(real2.l);
			return bBigInteger.compareTo(lBigInteger) < 0;
		}

		@Override
		public boolean lessThan(final BigIntegerStruct real2) {
			final BigInteger bBigInteger1 = real1.bigInteger;
			final BigInteger bBigInteger2 = real2.bigInteger;
			return bBigInteger1.compareTo(bBigInteger2) < 0;
		}

		@Override
		public boolean lessThan(final SingleFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) < 0;
		}

		@Override
		public boolean lessThan(final DoubleFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) < 0;
		}

		@Override
		public boolean lessThan(final BigFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimal;
			return bBigDecimal.compareTo(bigDecimal) < 0;
		}

		@Override
		public boolean lessThan(final RatioStruct real2) {
			final BigFraction bBigFraction = new BigFraction(real1.bigInteger);
			final BigFraction bigFraction = real2.bigFraction;
			return bBigFraction.compareTo(bigFraction) < 0;
		}
	}

	/**
	 * {@link RealStruct.GreaterThanVisitor} for computing numeric {@literal '>'} equality results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerGreaterThanVisitor extends RealStruct.GreaterThanVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerGreaterThanVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		private BigIntegerGreaterThanVisitor(final BigIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThan(final IntIntegerStruct real2) {
			final BigInteger bBigInteger = real1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(real2.i);
			return bBigInteger.compareTo(iBigInteger) > 0;
		}

		@Override
		public boolean greaterThan(final LongIntegerStruct real2) {
			final BigInteger bBigInteger = real1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(real2.l);
			return bBigInteger.compareTo(lBigInteger) > 0;
		}

		@Override
		public boolean greaterThan(final BigIntegerStruct real2) {
			final BigInteger bBigInteger1 = real1.bigInteger;
			final BigInteger bBigInteger2 = real2.bigInteger;
			return bBigInteger1.compareTo(bBigInteger2) > 0;
		}

		@Override
		public boolean greaterThan(final SingleFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) > 0;
		}

		@Override
		public boolean greaterThan(final DoubleFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) > 0;
		}

		@Override
		public boolean greaterThan(final BigFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimal;
			return bBigDecimal.compareTo(bigDecimal) > 0;
		}

		@Override
		public boolean greaterThan(final RatioStruct real2) {
			final BigFraction bBigFraction = new BigFraction(real1.bigInteger);
			final BigFraction bigFraction = real2.bigFraction;
			return bBigFraction.compareTo(bigFraction) > 0;
		}
	}

	/**
	 * {@link RealStruct.LessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerLessThanOrEqualToVisitor extends RealStruct.LessThanOrEqualToVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLessThanOrEqualToVisitor with the provided
		 * {@link BigIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		private BigIntegerLessThanOrEqualToVisitor(final BigIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThanOrEqualTo(final IntIntegerStruct real2) {
			final BigInteger bBigInteger = real1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(real2.i);
			return bBigInteger.compareTo(iBigInteger) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final LongIntegerStruct real2) {
			final BigInteger bBigInteger = real1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(real2.l);
			return bBigInteger.compareTo(lBigInteger) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final BigIntegerStruct real2) {
			final BigInteger bBigInteger1 = real1.bigInteger;
			final BigInteger bBigInteger2 = real2.bigInteger;
			return bBigInteger1.compareTo(bBigInteger2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final SingleFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final DoubleFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final BigFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimal;
			return bBigDecimal.compareTo(bigDecimal) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			final BigFraction bBigFraction = new BigFraction(real1.bigInteger);
			final BigFraction bigFraction = real2.bigFraction;
			return bBigFraction.compareTo(bigFraction) <= 0;
		}
	}

	/**
	 * {@link RealStruct.GreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerGreaterThanOrEqualToVisitor extends RealStruct.GreaterThanOrEqualToVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerGreaterThanOrEqualToVisitor with the
		 * provided {@link BigIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		private BigIntegerGreaterThanOrEqualToVisitor(final BigIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThanOrEqualTo(final IntIntegerStruct real2) {
			final BigInteger bBigInteger = real1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(real2.i);
			return bBigInteger.compareTo(iBigInteger) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final LongIntegerStruct real2) {
			final BigInteger bBigInteger = real1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(real2.l);
			return bBigInteger.compareTo(lBigInteger) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigIntegerStruct real2) {
			final BigInteger bBigInteger1 = real1.bigInteger;
			final BigInteger bBigInteger2 = real2.bigInteger;
			return bBigInteger1.compareTo(bBigInteger2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final SingleFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final DoubleFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return bBigDecimal.compareTo(bigDecimal) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigFloatStruct real2) {
			final BigDecimal bBigDecimal = new BigDecimal(real1.bigInteger);
			final BigDecimal bigDecimal = real2.bigDecimal;
			return bBigDecimal.compareTo(bigDecimal) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			final BigFraction bBigFraction = new BigFraction(real1.bigInteger);
			final BigFraction bigFraction = real2.bigFraction;
			return bBigFraction.compareTo(bigFraction) >= 0;
		}
	}

	/**
	 * {@link RealStruct.RealExptVisitor} for computing exponential function results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerExptVisitor extends RealStruct.RealExptVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerExptVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param base
		 * 		the base argument in the exponential operation
		 */
		private BigIntegerExptVisitor(final BigIntegerStruct base) {
			super(base);
		}

		@Override
		public NumberStruct expt(final IntIntegerStruct power) {
			if (power.minusp()) {
				// TODO: more efficient?
				return exptInteger(base, power);
			}

			final BigInteger baseBigInteger = base.bigInteger;
			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, power.i);
			return IntegerStruct.valueOf(pow);
		}

		@Override
		public NumberStruct expt(final LongIntegerStruct power) {
			if (power.minusp()) {
				// TODO: more efficient?
				return exptInteger(base, power);
			}

			final BigInteger baseBigInteger = base.bigInteger;
			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, power.l);
			return IntegerStruct.valueOf(pow);
		}

		@Override
		public NumberStruct expt(final BigIntegerStruct power) {
			if (power.minusp()) {
				// TODO: more efficient?
				return exptInteger(base, power);
			}

			final BigInteger baseBigInteger = base.bigInteger;
			final BigInteger powerBigInteger = power.bigInteger;
			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, powerBigInteger);
			return IntegerStruct.valueOf(pow);
		}

		@Override
		public NumberStruct expt(final SingleFloatStruct power) {
			// TODO: more efficient?
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
			return exptFloatRatioNew(base.bigInteger.doubleValue(), power.f);
		}

		@Override
		public NumberStruct expt(final DoubleFloatStruct power) {
			// TODO: more efficient?
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
			return exptFloatRatioNew(base.bigInteger.doubleValue(), power.d);
		}

		@Override
		public NumberStruct expt(final BigFloatStruct power) {
			// TODO: more efficient?
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
			return exptFloatRatioNew(base.bigInteger.doubleValue(), power.doubleValue());
		}

		@Override
		public NumberStruct expt(final RatioStruct power) {
			// TODO: more efficient?
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
			return exptFloatRatioNew(base.bigInteger.doubleValue(), power.doubleValue());
		}
	}

	/**
	 * {@link IntegerStruct.GcdVisitor} for computing greatest-common-denominator for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerGcdVisitor extends IntegerStruct.GcdVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerGcdVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the greatest-common-denominator operation
		 */
		private BigIntegerGcdVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct gcd(final IntIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);
			final BigInteger gcd = bBigInteger.gcd(iBigInteger);
			return IntegerStruct.valueOf(gcd);
		}

		@Override
		public IntegerStruct gcd(final LongIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);
			final BigInteger gcd = bBigInteger.gcd(lBigInteger);
			return IntegerStruct.valueOf(gcd);
		}

		@Override
		public IntegerStruct gcd(final BigIntegerStruct integer2) {
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;
			final BigInteger gcd = bBigInteger1.gcd(bBigInteger2);
			return IntegerStruct.valueOf(gcd);
		}
	}

	/**
	 * {@link IntegerStruct.LcmVisitor} for computing least-common-multiple for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerLcmVisitor extends IntegerStruct.LcmVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLcmVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the least-common-multiple operation
		 */
		private BigIntegerLcmVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct lcm(final IntIntegerStruct integer2) {
			if (integer1.zerop() || integer2.zerop()) {
				return ZERO;
			}

			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);

			final BigInteger multiply = bBigInteger.multiply(iBigInteger);
			final BigInteger abs = multiply.abs();
			final BigInteger gcd = bBigInteger.gcd(iBigInteger);
			final BigInteger divide = abs.divide(gcd);

			return IntegerStruct.valueOf(divide);
		}

		@Override
		public IntegerStruct lcm(final LongIntegerStruct integer2) {
			if (integer1.zerop() || integer2.zerop()) {
				return ZERO;
			}

			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);

			final BigInteger multiply = bBigInteger.multiply(lBigInteger);
			final BigInteger abs = multiply.abs();
			final BigInteger gcd = bBigInteger.gcd(lBigInteger);
			final BigInteger divide = abs.divide(gcd);

			return IntegerStruct.valueOf(divide);
		}

		@Override
		public IntegerStruct lcm(final BigIntegerStruct integer2) {
			if (integer1.zerop() || integer2.zerop()) {
				return ZERO;
			}

			// lcm(x y) = abs(x * y) / gcd(x y)
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;

			final BigInteger multiply = bBigInteger1.multiply(bBigInteger2);
			final BigInteger abs = multiply.abs();
			final BigInteger gcd = bBigInteger1.gcd(bBigInteger2);
			final BigInteger divide = abs.divide(gcd);

			return IntegerStruct.valueOf(divide);
		}
	}

	/**
	 * {@link IntegerStruct.AshVisitor} for performing bit-shifting operations for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerAshVisitor extends IntegerStruct.AshVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerAshVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer
		 * 		the integer argument in the bit-shifting operation
		 */
		private BigIntegerAshVisitor(final BigIntegerStruct integer) {
			super(integer);
		}

		@Override
		public IntegerStruct ash(final IntIntegerStruct count) {
			if (count.zerop()) {
				return integer;
			}
			final int countI = count.i;

			// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
			final BigInteger shiftedBigInteger = integer.bigInteger.shiftLeft(countI);
			return IntegerStruct.valueOf(shiftedBigInteger);
		}

		@Override
		public IntegerStruct ash(final LongIntegerStruct count) {
			if (count.zerop()) {
				return integer;
			}
			final int countI = count.intValue();

			// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
			final BigInteger shiftedBigInteger = integer.bigInteger.shiftLeft(countI);
			return IntegerStruct.valueOf(shiftedBigInteger);
		}

		@Override
		public IntegerStruct ash(final BigIntegerStruct count) {
			if (count.zerop()) {
				return integer;
			}
			final BigInteger countBigInteger = count.bigInteger;

			int countI;
			try {
				countI = countBigInteger.intValueExact();
			} catch (final ArithmeticException ignore) {
				if (LOGGER.isWarnEnabled()) {
					LOGGER.warn("Forcibly migrated {} to an int for bit-shifting.", countBigInteger);
				}
				countI = countBigInteger.intValue();
			}

			// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
			final BigInteger shiftedBigInteger = integer.bigInteger.shiftLeft(countI);
			return IntegerStruct.valueOf(shiftedBigInteger);
		}
	}

	/**
	 * {@link IntegerStruct.LogAndVisitor} for computing bitwise and results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerLogAndVisitor extends IntegerStruct.LogAndVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLogAndVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise and operation
		 */
		private BigIntegerLogAndVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAnd(final IntIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);
			return IntegerStruct.valueOf(bBigInteger.and(iBigInteger));
		}

		@Override
		public IntegerStruct logAnd(final LongIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);
			return IntegerStruct.valueOf(bBigInteger.and(lBigInteger));
		}

		@Override
		public IntegerStruct logAnd(final BigIntegerStruct integer2) {
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bBigInteger1.and(bBigInteger2));
		}
	}

	/**
	 * {@link IntegerStruct.LogAndC1Visitor} for computing bitwise and, with complementary first, results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerLogAndC1Visitor extends IntegerStruct.LogAndC1Visitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLogAndC1Visitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument, to be complementary, in the bitwise and operation
		 */
		private BigIntegerLogAndC1Visitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAndC1(final IntIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);
			return IntegerStruct.valueOf(bBigInteger.not().and(iBigInteger));
		}

		@Override
		public IntegerStruct logAndC1(final LongIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);
			return IntegerStruct.valueOf(bBigInteger.not().and(lBigInteger));
		}

		@Override
		public IntegerStruct logAndC1(final BigIntegerStruct integer2) {
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bBigInteger1.not().and(bBigInteger2));
		}
	}

	/**
	 * {@link IntegerStruct.LogAndC2Visitor} for computing bitwise and, with complementary second, results for {@link
	 * BigIntegerStruct}s.
	 */
	private static final class BigIntegerLogAndC2Visitor extends IntegerStruct.LogAndC2Visitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLogAndC2Visitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise and, with complementary second, operation
		 */
		private BigIntegerLogAndC2Visitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAndC2(final IntIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);
			return IntegerStruct.valueOf(bBigInteger.and(iBigInteger.not()));
		}

		@Override
		public IntegerStruct logAndC2(final LongIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);
			return IntegerStruct.valueOf(bBigInteger.and(lBigInteger.not()));
		}

		@Override
		public IntegerStruct logAndC2(final BigIntegerStruct integer2) {
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bBigInteger1.and(bBigInteger2.not()));
		}
	}

	/**
	 * {@link IntegerStruct.LogEqvVisitor} for computing bitwise exclusive-nor results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerLogEqvVisitor extends IntegerStruct.LogEqvVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLogEqvVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise exclusive-nor operation
		 */
		private BigIntegerLogEqvVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logEqv(final IntIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);
			final BigInteger xor = bBigInteger.xor(iBigInteger);
			return IntegerStruct.valueOf(xor.not());
		}

		@Override
		public IntegerStruct logEqv(final LongIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);
			final BigInteger xor = bBigInteger.xor(lBigInteger);
			return IntegerStruct.valueOf(xor.not());
		}

		@Override
		public IntegerStruct logEqv(final BigIntegerStruct integer2) {
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;
			final BigInteger xor = bBigInteger1.xor(bBigInteger2);
			return IntegerStruct.valueOf(xor.not());
		}
	}

	/**
	 * {@link IntegerStruct.LogIorVisitor} for computing bitwise inclusive-or results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerLogIorVisitor extends IntegerStruct.LogIorVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLogIorVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise inclusive-or operation
		 */
		private BigIntegerLogIorVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logIor(final IntIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);
			return IntegerStruct.valueOf(bBigInteger.or(iBigInteger));
		}

		@Override
		public IntegerStruct logIor(final LongIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);
			return IntegerStruct.valueOf(bBigInteger.or(lBigInteger));
		}

		@Override
		public IntegerStruct logIor(final BigIntegerStruct integer2) {
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bBigInteger1.or(bBigInteger2));
		}
	}

	/**
	 * {@link IntegerStruct.LogNandVisitor} for computing bitwise nand results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerLogNandVisitor extends IntegerStruct.LogNandVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLogNandVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise nand operation
		 */
		private BigIntegerLogNandVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logNand(final IntIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);
			final BigInteger and = bBigInteger.and(iBigInteger);
			return IntegerStruct.valueOf(and.not());
		}

		@Override
		public IntegerStruct logNand(final LongIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);
			final BigInteger and = bBigInteger.and(lBigInteger);
			return IntegerStruct.valueOf(and.not());
		}

		@Override
		public IntegerStruct logNand(final BigIntegerStruct integer2) {
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;
			final BigInteger and = bBigInteger1.and(bBigInteger2);
			return IntegerStruct.valueOf(and.not());
		}
	}

	/**
	 * {@link IntegerStruct.LogNorVisitor} for computing bitwise inclusive-nor results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerLogNorVisitor extends IntegerStruct.LogNorVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLogNorVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise inclusive-nor operation
		 */
		private BigIntegerLogNorVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logNor(final IntIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);
			final BigInteger or = bBigInteger.or(iBigInteger);
			return IntegerStruct.valueOf(or.not());
		}

		@Override
		public IntegerStruct logNor(final LongIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);
			final BigInteger or = bBigInteger.or(lBigInteger);
			return IntegerStruct.valueOf(or.not());
		}

		@Override
		public IntegerStruct logNor(final BigIntegerStruct integer2) {
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;
			final BigInteger or = bBigInteger1.or(bBigInteger2);
			return IntegerStruct.valueOf(or.not());
		}
	}

	/**
	 * {@link IntegerStruct.LogOrC1Visitor} for computing bitwise inclusive-or, with complementary first, results for
	 * {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerLogOrC1Visitor extends IntegerStruct.LogOrC1Visitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLogOrC1Visitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument, to be complementary, in the bitwise inclusive-or operation
		 */
		private BigIntegerLogOrC1Visitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logOrC1(final IntIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);
			return IntegerStruct.valueOf(bBigInteger.not().or(iBigInteger));
		}

		@Override
		public IntegerStruct logOrC1(final LongIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);
			return IntegerStruct.valueOf(bBigInteger.not().or(lBigInteger));
		}

		@Override
		public IntegerStruct logOrC1(final BigIntegerStruct integer2) {
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bBigInteger1.not().or(bBigInteger2));
		}
	}

	/**
	 * {@link IntegerStruct.LogOrC2Visitor} for computing bitwise inclusive-or, with complementary second, results for
	 * {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerLogOrC2Visitor extends IntegerStruct.LogOrC2Visitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLogOrC2Visitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise inclusive-or, with complementary second, operation
		 */
		private BigIntegerLogOrC2Visitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logOrC2(final IntIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);
			return IntegerStruct.valueOf(bBigInteger.or(iBigInteger.not()));
		}

		@Override
		public IntegerStruct logOrC2(final LongIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);
			return IntegerStruct.valueOf(bBigInteger.or(lBigInteger.not()));
		}

		@Override
		public IntegerStruct logOrC2(final BigIntegerStruct integer2) {
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bBigInteger1.or(bBigInteger2.not()));
		}
	}

	/**
	 * {@link IntegerStruct.LogXorVisitor} for computing bitwise exclusive-or results for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerLogXorVisitor extends IntegerStruct.LogXorVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLogXorVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise exclusive-or operation
		 */
		private BigIntegerLogXorVisitor(final BigIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logXor(final IntIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger iBigInteger = BigInteger.valueOf(integer2.i);
			return IntegerStruct.valueOf(bBigInteger.xor(iBigInteger));
		}

		@Override
		public IntegerStruct logXor(final LongIntegerStruct integer2) {
			final BigInteger bBigInteger = integer1.bigInteger;
			final BigInteger lBigInteger = BigInteger.valueOf(integer2.l);
			return IntegerStruct.valueOf(bBigInteger.xor(lBigInteger));
		}

		@Override
		public IntegerStruct logXor(final BigIntegerStruct integer2) {
			final BigInteger bBigInteger1 = integer1.bigInteger;
			final BigInteger bBigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bBigInteger1.xor(bBigInteger2));
		}
	}

	/**
	 * {@link IntegerStruct.LogBitPVisitor} for testing the active bit by index for {@link BigIntegerStruct}s.
	 */
	private static final class BigIntegerLogBitPVisitor extends IntegerStruct.LogBitPVisitor<BigIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an BigIntegerLogBitPVisitor with the provided {@link
		 * BigIntegerStruct}.
		 *
		 * @param integer
		 * 		the {@link BigIntegerStruct} to perform the active bit by index test
		 */
		private BigIntegerLogBitPVisitor(final BigIntegerStruct integer) {
			super(integer);
		}

		@Override
		public boolean logBitP(final IntIntegerStruct index) {
			final BigInteger bBigInteger = integer.bigInteger;
			final int indexInt = index.intValue();
			return bBigInteger.testBit(indexInt);
		}

		@Override
		public boolean logBitP(final LongIntegerStruct index) {
			final BigInteger bBigInteger = integer.bigInteger;
			final int indexInt = index.intValue();
			return bBigInteger.testBit(indexInt);
		}

		@Override
		public boolean logBitP(final BigIntegerStruct index) {
			final BigInteger bBigInteger = integer.bigInteger;
			final int indexInt = index.intValue();
			return bBigInteger.testBit(indexInt);
		}
	}
}

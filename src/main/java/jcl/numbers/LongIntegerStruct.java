/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

import com.google.common.math.LongMath;
import jcl.classes.BuiltInClassStruct;
import jcl.conditions.exceptions.DivisionByZeroException;
import jcl.types.BignumType;
import jcl.util.NumberUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.exception.MathArithmeticException;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link LongIntegerStruct} is the object representation of a Lisp 'integer' type that can be contained within a
 * Java {@code long}.
 */
public final class LongIntegerStruct extends BuiltInClassStruct implements IntegerStruct {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(LongIntegerStruct.class);

	/**
	 * The internal {@code long} containing the LongIntegerStruct contents.
	 */
	final long l;

	/**
	 * Private constructor.
	 *
	 * @param l
	 * 		the value of the LongIntegerStruct
	 */
	private LongIntegerStruct(final long l) {
		super(BignumType.INSTANCE, null, null);
		this.l = l;
	}

	/**
	 * Returns a LongIntegerStruct object with the provided {@code long} value.
	 *
	 * @param l
	 * 		the int value of the resulting LongIntegerStruct
	 *
	 * @return a LongIntegerStruct object with the provided {@code long} value
	 */
	public static LongIntegerStruct valueOf(final long l) {
		return new LongIntegerStruct(l);
	}

	/*
		IntegerStruct
	 */

	@Override
	public int intValue() {
		if (!NumberUtils.longFitsInInt(l)) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
		}
		return NumberUtils.longToInt(l);
	}

	@Override
	public long longValue() {
		return l;
	}

	@Override
	public BigInteger bigIntegerValue() {
		return BigInteger.valueOf(l);
	}

	@Override
	public IntegerStruct gcd(final IntegerStruct.GcdVisitor<?> gcdVisitor) {
		return gcdVisitor.gcd(this);
	}

	@Override
	public IntegerStruct.GcdVisitor<?> gcdVisitor() {
		return new LongIntegerGcdVisitor(this);
	}

	@Override
	public IntegerStruct lcm(final IntegerStruct.LcmVisitor<?> lcmVisitor) {
		return lcmVisitor.lcm(this);
	}

	@Override
	public IntegerStruct.LcmVisitor<?> lcmVisitor() {
		return new LongIntegerLcmVisitor(this);
	}

	@Override
	public IntegerStruct ash(final AshVisitor<?> ashVisitor) {
		return ashVisitor.ash(this);
	}

	@Override
	public AshVisitor<?> ashVisitor() {
		return new LongIntegerAshVisitor(this);
	}

	@Override
	public IntegerStruct logAnd(final LogAndVisitor<?> logAndVisitor) {
		return logAndVisitor.logAnd(this);
	}

	@Override
	public LogAndVisitor<?> logAndVisitor() {
		return new LongIntegerLogAndVisitor(this);
	}

	@Override
	public IntegerStruct logAndC1(final LogAndC1Visitor<?> logAndC1Visitor) {
		return logAndC1Visitor.logAndC1(this);
	}

	@Override
	public LogAndC1Visitor<?> logAndC1Visitor() {
		return new LongIntegerLogAndC1Visitor(this);
	}

	@Override
	public IntegerStruct logAndC2(final LogAndC2Visitor<?> logAndC2Visitor) {
		return logAndC2Visitor.logAndC2(this);
	}

	@Override
	public LogAndC2Visitor<?> logAndC2Visitor() {
		return new LongIntegerLogAndC2Visitor(this);
	}

	@Override
	public IntegerStruct logEqv(final LogEqvVisitor<?> logEqvVisitor) {
		return logEqvVisitor.logEqv(this);
	}

	@Override
	public LogEqvVisitor<?> logEqvVisitor() {
		return new LongIntegerLogEqvVisitor(this);
	}

	@Override
	public IntegerStruct logIor(final LogIorVisitor<?> logIorVisitor) {
		return logIorVisitor.logIor(this);
	}

	@Override
	public LogIorVisitor<?> logIorVisitor() {
		return new LongIntegerLogIorVisitor(this);
	}

	@Override
	public IntegerStruct logNand(final LogNandVisitor<?> logNandVisitor) {
		return logNandVisitor.logNand(this);
	}

	@Override
	public LogNandVisitor<?> logNandVisitor() {
		return new LongIntegerLogNandVisitor(this);
	}

	@Override
	public IntegerStruct logNor(final LogNorVisitor<?> logNorVisitor) {
		return logNorVisitor.logNor(this);
	}

	@Override
	public LogNorVisitor<?> logNorVisitor() {
		return new LongIntegerLogNorVisitor(this);
	}

	@Override
	public IntegerStruct logNot() {
		return IntegerStruct.valueOf(~l);
	}

	@Override
	public IntegerStruct logOrC1(final LogOrC1Visitor<?> logOrC1Visitor) {
		return logOrC1Visitor.logOrC1(this);
	}

	@Override
	public LogOrC1Visitor<?> logOrC1Visitor() {
		return new LongIntegerLogOrC1Visitor(this);
	}

	@Override
	public IntegerStruct logOrC2(final LogOrC2Visitor<?> logOrC2Visitor) {
		return logOrC2Visitor.logOrC2(this);
	}

	@Override
	public LogOrC2Visitor<?> logOrC2Visitor() {
		return new LongIntegerLogOrC2Visitor(this);
	}

	@Override
	public IntegerStruct logXor(final LogXorVisitor<?> logXorVisitor) {
		return logXorVisitor.logXor(this);
	}

	@Override
	public LogXorVisitor<?> logXorVisitor() {
		return new LongIntegerLogXorVisitor(this);
	}

	@Override
	public boolean logBitP(final LogBitPVisitor<?> logBitPVisitor) {
		return logBitPVisitor.logBitP(this);
	}

	@Override
	public LogBitPVisitor<?> logBitPVisitor() {
		return new LongIntegerLogBitPVisitor(this);
	}

	@Override
	public IntegerStruct logCount() {
		final long bitCount = Long.bitCount(l);
		return IntegerStruct.valueOf(bitCount);
	}

	@Override
	public IntegerStruct integerLength() {
		final long correctedL = (l < 0) ? -l : (l + 1L);
		final long bitLength = LongMath.log2(correctedL, RoundingMode.CEILING);
		return IntegerStruct.valueOf(bitLength);
	}

	@Override
	public boolean evenp() {
		return (l % 2) == 0;
	}

	@Override
	public boolean oddp() {
		return (l % 2) != 0;
	}

	@Override
	public IntegerStruct isqrt() {
		final long sqrtFloor = LongMath.sqrt(l, RoundingMode.FLOOR);
		return IntegerStruct.valueOf(sqrtFloor);
	}

	/*
		RealStruct
	 */

	@Override
	public FloatStruct coerceRealToFloat() {
		return FloatStruct.valueOf(l);
	}

	@Override
	public boolean isLessThan(final RealStruct.LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	@Override
	public RealStruct.LessThanVisitor<?> lessThanVisitor() {
		return new LongIntegerLessThanVisitor(this);
	}

	@Override
	public boolean isGreaterThan(final RealStruct.GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	@Override
	public RealStruct.GreaterThanVisitor<?> greaterThanVisitor() {
		return new LongIntegerGreaterThanVisitor(this);
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	@Override
	public RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new LongIntegerLessThanOrEqualToVisitor(this);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	@Override
	public RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new LongIntegerGreaterThanOrEqualToVisitor(this);
	}

	@Override
	public boolean plusp() {
		return l > 0L;
	}

	@Override
	public boolean minusp() {
		return l < 0L;
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
		return new LongIntegerAddVisitor(this);
	}

	@Override
	public NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	public SubtractVisitor<?> subtractVisitor() {
		return new LongIntegerSubtractVisitor(this);
	}

	@Override
	public NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	public MultiplyVisitor<?> multiplyVisitor() {
		return new LongIntegerMultiplyVisitor(this);
	}

	@Override
	public NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	public DivideVisitor<?> divideVisitor() {
		return new LongIntegerDivideVisitor(this);
	}

	@Override
	public boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	@Override
	public EqualToVisitor<?> equalToVisitor() {
		return new LongIntegerEqualToVisitor(this);
	}

	@Override
	public NumberStruct expt(final ExptVisitor<?> exptVisitor) {
		return exptVisitor.expt(this);
	}

	@Override
	public ExptVisitor<?> exptVisitor() {
		return new LongIntegerExptVisitor(this);
	}

	@Override
	public boolean zerop() {
		return l == 0L;
	}

	@Override
	public RealStruct abs() {
		if (l >= 0L) {
			return this;
		}
		return negation();
	}

	@Override
	public IntegerStruct negation() {
		return valueOf(-l);
	}

	@Override
	public NumberStruct reciprocal() {
		if (l == 0L) {
			throw new DivisionByZeroException("Division by zero.");
		}
		if (l == 1L) {
			return this;
		}
		return RatioStruct.valueOf(BigInteger.ONE, BigInteger.valueOf(l));
	}

	// HashCode / Equals

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(l)
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
		final LongIntegerStruct rhs = (LongIntegerStruct) obj;
		return new EqualsBuilder().append(l, rhs.l)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return String.valueOf(l);
	}

	// Visitor Implementations

	/**
	 * {@link RealStruct.RealAddVisitor} for computing addition results for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerAddVisitor extends RealStruct.RealAddVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerAddVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		private LongIntegerAddVisitor(final LongIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct add(final IntIntegerStruct number2) {
			final long l1 = number1.l;
			final int i = number2.i;
			return IntegerStruct.valueOf(l1 + i);
		}

		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			final long l1 = number1.l;
			final long l2 = number2.l;
			return IntegerStruct.valueOf(l1 + l2);
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			final BigInteger lBigInteger = BigInteger.valueOf(number1.l);
			final BigInteger bigInteger = number2.bigInteger;
			final BigInteger add = lBigInteger.add(bigInteger);
			return BigIntegerStruct.valueOf(add);
		}

		@Override
		public RealStruct add(final SingleFloatStruct number2) {
			final long l1 = number1.l;
			final float f = number2.f;
			return SingleFloatStruct.valueOf(l1 + f);
		}

		@Override
		public RealStruct add(final DoubleFloatStruct number2) {
			final long l1 = number1.l;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(l1 + d);
		}

		@Override
		public RealStruct add(final BigFloatStruct number2) {
			final BigDecimal lBigDecimal = BigDecimal.valueOf(number1.l);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal add = lBigDecimal.add(bigDecimal);
			return BigFloatStruct.valueOf(add);
		}

		@Override
		public RealStruct add(final RatioStruct number2) {
			final long l1 = number1.l;
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction add = bigFraction.add(l1);
			return RationalStruct.makeRational(add);
		}

		@Override
		public NumberStruct add(final ComplexStruct number2) {
			// TODO
			return super.add(number2);
		}
	}

	/**
	 * {@link RealStruct.RealSubtractVisitor} for computing subtraction function results for {@link
	 * LongIntegerStruct}s.
	 */
	private static final class LongIntegerSubtractVisitor extends RealStruct.RealSubtractVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerSubtractVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		private LongIntegerSubtractVisitor(final LongIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct subtract(final IntIntegerStruct number2) {
			final long l1 = number1.l;
			final int i = number2.i;
			return IntegerStruct.valueOf(l1 - i);
		}

		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			final long l1 = number1.l;
			final long l2 = number2.l;
			return IntegerStruct.valueOf(l1 - l2);
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			final BigInteger lBigInteger = BigInteger.valueOf(number1.l);
			final BigInteger bigInteger = number2.bigInteger;
			final BigInteger subtract = lBigInteger.subtract(bigInteger);
			return IntegerStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final SingleFloatStruct number2) {
			final long l1 = number1.l;
			final float f = number2.f;
			return SingleFloatStruct.valueOf(l1 - f);
		}

		@Override
		public RealStruct subtract(final DoubleFloatStruct number2) {
			final long l1 = number1.l;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(l1 - d);
		}

		@Override
		public RealStruct subtract(final BigFloatStruct number2) {
			final BigDecimal lBigDecimal = BigDecimal.valueOf(number1.l);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal subtract = lBigDecimal.subtract(bigDecimal);
			return BigFloatStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final BigFraction lBigFraction = new BigFraction(number1.l);
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction subtract = lBigFraction.subtract(bigFraction);
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
	 * LongIntegerStruct}s.
	 */
	private static final class LongIntegerMultiplyVisitor extends RealStruct.RealMultiplyVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerMultiplyVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		private LongIntegerMultiplyVisitor(final LongIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct multiply(final IntIntegerStruct number2) {
			final long l1 = number1.l;
			final int i = number2.i;
			return IntegerStruct.valueOf(l1 * i);
		}

		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			final long l1 = number1.l;
			final long l2 = number2.l;
			return IntegerStruct.valueOf(l1 * l2);
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			final BigInteger lBigInteger = BigInteger.valueOf(number1.l);
			final BigInteger bigInteger = number2.bigInteger;
			final BigInteger multiply = lBigInteger.multiply(bigInteger);
			return BigIntegerStruct.valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final SingleFloatStruct number2) {
			final long l1 = number1.l;
			final float f = number2.f;
			return SingleFloatStruct.valueOf(l1 * f);
		}

		@Override
		public RealStruct multiply(final DoubleFloatStruct number2) {
			final long l1 = number1.l;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(l1 * d);
		}

		@Override
		public RealStruct multiply(final BigFloatStruct number2) {
			final BigDecimal lBigDecimal = BigDecimal.valueOf(number1.l);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal multiply = lBigDecimal.multiply(bigDecimal);
			return BigFloatStruct.valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final long l1 = number1.l;
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction multiply = bigFraction.multiply(l1);
			return RationalStruct.makeRational(multiply);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number2) {
			// TODO
			return super.multiply(number2);
		}
	}

	/**
	 * {@link RealStruct.RealDivideVisitor} for computing division function results for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerDivideVisitor extends RealStruct.RealDivideVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerDivideVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		private LongIntegerDivideVisitor(final LongIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct divide(final IntIntegerStruct number2) {
			final BigInteger bigInteger1 = BigInteger.valueOf(number1.l);
			final BigInteger bigInteger2 = BigInteger.valueOf(number2.i);
			return RationalStruct.makeRational(bigInteger1, bigInteger2);
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			final BigInteger bigInteger1 = BigInteger.valueOf(number1.l);
			final BigInteger bigInteger2 = BigInteger.valueOf(number2.l);
			return RationalStruct.makeRational(bigInteger1, bigInteger2);
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			final BigInteger bigInteger1 = BigInteger.valueOf(number1.l);
			final BigInteger bigInteger2 = number2.bigInteger;
			return RationalStruct.makeRational(bigInteger1, bigInteger2);
		}

		@Override
		public RealStruct divide(final SingleFloatStruct number2) {
			final long l1 = number1.l;
			final float f = number2.f;
			return SingleFloatStruct.valueOf(l1 / f);
		}

		@Override
		public RealStruct divide(final DoubleFloatStruct number2) {
			final long l1 = number1.l;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(l1 / d);
		}

		@Override
		public RealStruct divide(final BigFloatStruct number2) {
			final BigDecimal lBigDecimal = BigDecimal.valueOf(number1.l);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal divide = lBigDecimal.divide(bigDecimal, MathContext.DECIMAL128);
			return BigFloatStruct.valueOf(divide);
		}

		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigFraction lBigFraction = new BigFraction(number1.l);
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction divide = lBigFraction.divide(bigFraction);
			return RationalStruct.makeRational(divide);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			// TODO
			return super.divide(number2);
		}
	}

	/**
	 * {@link RationalStruct.RationalEqualToVisitor} for computing numeric '=' equality results for {@link
	 * LongIntegerStruct}s.
	 */
	private static final class LongIntegerEqualToVisitor extends RationalStruct.RationalEqualToVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerEqualToVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		private LongIntegerEqualToVisitor(final LongIntegerStruct number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final IntIntegerStruct number2) {
			return number1.l == number2.i;
		}

		@Override
		public boolean equalTo(final LongIntegerStruct number2) {
			return number1.l == number2.l;
		}

		@Override
		public boolean equalTo(final BigIntegerStruct number2) {
			final BigInteger lBigInteger = BigInteger.valueOf(number1.l);
			final BigInteger bigInteger = number2.bigInteger;
			return lBigInteger.compareTo(bigInteger) == 0;
		}

		@Override
		public boolean equalTo(final RatioStruct number2) {
			final BigFraction lBigFraction = new BigFraction(number1.l);
			final BigFraction bigFraction = number2.bigFraction;
			return lBigFraction.equals(bigFraction);
		}
	}

	/**
	 * {@link RationalStruct.RationalLessThanVisitor} for computing numeric {@literal '<'} equality results for {@link
	 * LongIntegerStruct}s.
	 */
	private static final class LongIntegerLessThanVisitor extends RationalStruct.RationalLessThanVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLessThanVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		private LongIntegerLessThanVisitor(final LongIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThan(final IntIntegerStruct real2) {
			return real1.l < real2.i;
		}

		@Override
		public boolean lessThan(final LongIntegerStruct real2) {
			return real1.l < real2.l;
		}

		@Override
		public boolean lessThan(final BigIntegerStruct real2) {
			final BigInteger lBigInteger = BigInteger.valueOf(real1.l);
			final BigInteger bigInteger = real2.bigInteger;
			return lBigInteger.compareTo(bigInteger) < 0;
		}

		@Override
		public boolean lessThan(final RatioStruct real2) {
			final BigFraction lBigFraction = new BigFraction(real1.l);
			final BigFraction bigFraction = real2.bigFraction;
			return lBigFraction.compareTo(bigFraction) < 0;
		}
	}

	/**
	 * {@link RationalStruct.RationalGreaterThanVisitor} for computing numeric {@literal '>'} equality results for
	 * {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerGreaterThanVisitor extends RationalStruct.RationalGreaterThanVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerGreaterThanVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		private LongIntegerGreaterThanVisitor(final LongIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThan(final IntIntegerStruct real2) {
			return real1.l > real2.i;
		}

		@Override
		public boolean greaterThan(final LongIntegerStruct real2) {
			return real1.l > real2.l;
		}

		@Override
		public boolean greaterThan(final BigIntegerStruct real2) {
			final BigInteger lBigInteger = BigInteger.valueOf(real1.l);
			final BigInteger bigInteger = real2.bigInteger;
			return lBigInteger.compareTo(bigInteger) > 0;
		}

		@Override
		public boolean greaterThan(final RatioStruct real2) {
			final BigFraction lBigFraction = new BigFraction(real1.l);
			final BigFraction bigFraction = real2.bigFraction;
			return lBigFraction.compareTo(bigFraction) > 0;
		}
	}

	/**
	 * {@link RationalStruct.RationalLessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality results
	 * for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerLessThanOrEqualToVisitor extends RationalStruct.RationalLessThanOrEqualToVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLessThanOrEqualToVisitor with the provided
		 * {@link LongIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		private LongIntegerLessThanOrEqualToVisitor(final LongIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThanOrEqualTo(final IntIntegerStruct real2) {
			return real1.l <= real2.i;
		}

		@Override
		public boolean lessThanOrEqualTo(final LongIntegerStruct real2) {
			return real1.l <= real2.l;
		}

		@Override
		public boolean lessThanOrEqualTo(final BigIntegerStruct real2) {
			final BigInteger lBigInteger = BigInteger.valueOf(real1.l);
			final BigInteger bigInteger = real2.bigInteger;
			return lBigInteger.compareTo(bigInteger) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			final BigFraction lBigFraction = new BigFraction(real1.l);
			final BigFraction bigFraction = real2.bigFraction;
			return lBigFraction.compareTo(bigFraction) <= 0;
		}
	}

	/**
	 * {@link RationalStruct.RationalGreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality
	 * results for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerGreaterThanOrEqualToVisitor extends RationalStruct.RationalGreaterThanOrEqualToVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerGreaterThanOrEqualToVisitor with the
		 * provided {@link LongIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		private LongIntegerGreaterThanOrEqualToVisitor(final LongIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThanOrEqualTo(final IntIntegerStruct real2) {
			return real1.l >= real2.i;
		}

		@Override
		public boolean greaterThanOrEqualTo(final LongIntegerStruct real2) {
			return real1.l >= real2.l;
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigIntegerStruct real2) {
			final BigInteger lBigInteger = BigInteger.valueOf(real1.l);
			final BigInteger bigInteger = real2.bigInteger;
			return lBigInteger.compareTo(bigInteger) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			final BigFraction lBigFraction = new BigFraction(real1.l);
			final BigFraction bigFraction = real2.bigFraction;
			return lBigFraction.compareTo(bigFraction) >= 0;
		}
	}

	/**
	 * {@link RealStruct.RealExptVisitor} for computing exponential function results for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerExptVisitor extends RealStruct.RealExptVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerExptVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param base
		 * 		the base argument in the exponential operation
		 */
		private LongIntegerExptVisitor(final LongIntegerStruct base) {
			super(base);
		}

		@Override
		public NumberStruct expt(final IntIntegerStruct power) {
			if (power.minusp()) {
				// TODO: more efficient?
				return exptInteger(base, power);
			}

			try {
				final long pow = ArithmeticUtils.pow(base.l, power.i);
				return IntegerStruct.valueOf(pow);
			} catch (final MathArithmeticException ignore) {
			}

			final BigInteger baseBigInteger = BigInteger.valueOf(base.l);
			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, power.i);
			return IntegerStruct.valueOf(pow);
		}

		@Override
		@SuppressWarnings("deprecation")
		public NumberStruct expt(final LongIntegerStruct power) {
			if (power.minusp()) {
				// TODO: more efficient?
				return exptInteger(base, power);
			}

			try {
				final long pow = ArithmeticUtils.pow(base.l, power.l);
				return IntegerStruct.valueOf(pow);
			} catch (final MathArithmeticException ignore) {
			}

			final BigInteger baseBigInteger = BigInteger.valueOf(base.l);
			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, power.l);
			return IntegerStruct.valueOf(pow);
		}

		@Override
		public NumberStruct expt(final BigIntegerStruct power) {
			if (power.minusp()) {
				// TODO: more efficient?
				return exptInteger(base, power);
			}

			final BigInteger baseBigInteger = BigInteger.valueOf(base.l);
			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, power.bigInteger);
			return IntegerStruct.valueOf(pow);
		}

		@Override
		public NumberStruct expt(final SingleFloatStruct power) {
			// TODO: more efficient?
			return exptFloatRatioNew(base.l, power.f);
		}

		@Override
		public NumberStruct expt(final DoubleFloatStruct power) {
			// TODO: more efficient?
			return exptFloatRatioNew(base.l, power.d);
		}

		@Override
		public NumberStruct expt(final BigFloatStruct power) {
			// TODO: more efficient?
			return exptFloatRatioNew(base.l, power.doubleValue());
		}

		@Override
		public NumberStruct expt(final RatioStruct power) {
			// TODO: more efficient?
			return exptFloatRatioNew(base.l, power.doubleValue());
		}
	}

	/**
	 * {@link IntegerStruct.GcdVisitor} for computing greatest-common-denominator for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerGcdVisitor extends IntegerStruct.GcdVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerGcdVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the greatest-common-denominator operation
		 */
		private LongIntegerGcdVisitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct gcd(final IntIntegerStruct integer2) {
			final long gcd = ArithmeticUtils.gcd(integer1.l, integer2.i);
			return IntegerStruct.valueOf(gcd);
		}

		@Override
		public IntegerStruct gcd(final LongIntegerStruct integer2) {
			final long gcd = ArithmeticUtils.gcd(integer1.l, integer2.l);
			return IntegerStruct.valueOf(gcd);
		}

		@Override
		public IntegerStruct gcd(final BigIntegerStruct integer2) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;
			final BigInteger gcd = lBigInteger.gcd(bigInteger);
			return IntegerStruct.valueOf(gcd);
		}
	}

	/**
	 * {@link IntegerStruct.LcmVisitor} for computing least-common-multiple for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerLcmVisitor extends IntegerStruct.LcmVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLcmVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the least-common-multiple operation
		 */
		private LongIntegerLcmVisitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct lcm(final IntIntegerStruct integer2) {
			if (integer1.zerop() || integer2.zerop()) {
				return ZERO;
			}

			final long lcm = ArithmeticUtils.lcm(integer1.l, integer2.i);
			return IntegerStruct.valueOf(lcm);
		}

		@Override
		public IntegerStruct lcm(final LongIntegerStruct integer2) {
			if (integer1.zerop() || integer2.zerop()) {
				return ZERO;
			}

			final long lcm = ArithmeticUtils.lcm(integer1.l, integer2.l);
			return IntegerStruct.valueOf(lcm);
		}

		@Override
		public IntegerStruct lcm(final BigIntegerStruct integer2) {
			if (integer1.zerop() || integer2.zerop()) {
				return ZERO;
			}

			// lcm(x y) = abs(x * y) / gcd(x y)
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;

			final BigInteger multiply = lBigInteger.multiply(bigInteger);
			final BigInteger abs = multiply.abs();
			final BigInteger gcd = lBigInteger.gcd(bigInteger);
			final BigInteger divide = abs.divide(gcd);

			return IntegerStruct.valueOf(divide);
		}
	}

	/**
	 * {@link IntegerStruct.AshVisitor} for performing bit-shifting operations for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerAshVisitor extends IntegerStruct.AshVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerAshVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer
		 * 		the integer argument in the bit-shifting operation
		 */
		private LongIntegerAshVisitor(final LongIntegerStruct integer) {
			super(integer);
		}

		@Override
		public IntegerStruct ash(final IntIntegerStruct count) {
			if (count.zerop()) {
				return integer;
			}
			final int countI = count.i;

			final long shiftedL;
			if (countI > 0) {
				// Shift Left if Count is Positive
				shiftedL = integer.l << countI;
			} else {
				// Shift Right if Count is Negative
				shiftedL = integer.l >> countI;
			}
			return IntegerStruct.valueOf(shiftedL);
		}

		@Override
		public IntegerStruct ash(final LongIntegerStruct count) {
			if (count.zerop()) {
				return integer;
			}
			final long countL = count.l;

			final long shiftedL;
			if (countL > 0L) {
				// Shift Left if Count is Positive
				shiftedL = integer.l << countL;
			} else {
				// Shift Right if Count is Negative
				shiftedL = integer.l >> countL;
			}
			return IntegerStruct.valueOf(shiftedL);
		}

		@Override
		public IntegerStruct ash(final BigIntegerStruct count) {
			if (count.zerop()) {
				return integer;
			}
			final BigInteger countBigInteger = count.bigInteger;

			long countL;
			try {
				countL = countBigInteger.longValueExact();
			} catch (final ArithmeticException ignore) {
				if (LOGGER.isWarnEnabled()) {
					LOGGER.warn("Forcibly migrated {} to a long for bit-shifting.", countBigInteger);
				}
				countL = countBigInteger.longValue();
			}

			final long shiftedL;
			if (countL > 0L) {
				// Shift Left if Count is Positive
				shiftedL = integer.l << countL;
			} else {
				// Shift Right if Count is Negative
				shiftedL = integer.l >> countL;
			}
			return IntegerStruct.valueOf(shiftedL);
		}
	}

	/**
	 * {@link IntegerStruct.LogAndVisitor} for computing bitwise and results for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerLogAndVisitor extends IntegerStruct.LogAndVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLogAndVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise and operation
		 */
		private LongIntegerLogAndVisitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAnd(final IntIntegerStruct integer2) {
			final long l1 = integer1.l;
			final int i = integer2.i;
			return IntegerStruct.valueOf(l1 & i);
		}

		@Override
		public IntegerStruct logAnd(final LongIntegerStruct integer2) {
			final long l1 = integer1.l;
			final long l2 = integer2.l;
			return IntegerStruct.valueOf(l1 & l2);
		}

		@Override
		public IntegerStruct logAnd(final BigIntegerStruct integer2) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(lBigInteger.and(bigInteger));
		}
	}

	/**
	 * {@link IntegerStruct.LogAndC1Visitor} for computing bitwise and, with complementary first, results for {@link
	 * LongIntegerStruct}s.
	 */
	private static final class LongIntegerLogAndC1Visitor extends IntegerStruct.LogAndC1Visitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLogAndC1Visitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument, to be complementary, in the bitwise and operation
		 */
		private LongIntegerLogAndC1Visitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAndC1(final IntIntegerStruct integer2) {
			final long l1 = integer1.l;
			final int i = integer2.i;
			return IntegerStruct.valueOf(~l1 & i);
		}

		@Override
		public IntegerStruct logAndC1(final LongIntegerStruct integer2) {
			final long l1 = integer1.l;
			final long l2 = integer2.l;
			return IntegerStruct.valueOf(~l1 & l2);
		}

		@Override
		public IntegerStruct logAndC1(final BigIntegerStruct integer2) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(lBigInteger.not().and(bigInteger));
		}
	}

	/**
	 * {@link IntegerStruct.LogAndC2Visitor} for computing bitwise and, with complementary second, results for {@link
	 * LongIntegerStruct}s.
	 */
	private static final class LongIntegerLogAndC2Visitor extends IntegerStruct.LogAndC2Visitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLogAndC2Visitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise and, with complementary second, operation
		 */
		private LongIntegerLogAndC2Visitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAndC2(final IntIntegerStruct integer2) {
			final long l1 = integer1.l;
			final int i = integer2.i;
			return IntegerStruct.valueOf(l1 & ~i);
		}

		@Override
		public IntegerStruct logAndC2(final LongIntegerStruct integer2) {
			final long l1 = integer1.l;
			final long l2 = integer2.l;
			return IntegerStruct.valueOf(l1 & ~l2);
		}

		@Override
		public IntegerStruct logAndC2(final BigIntegerStruct integer2) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(lBigInteger.and(bigInteger.not()));
		}
	}

	/**
	 * {@link IntegerStruct.LogEqvVisitor} for computing bitwise exclusive-nor results for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerLogEqvVisitor extends IntegerStruct.LogEqvVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLogEqvVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise exclusive-nor operation
		 */
		private LongIntegerLogEqvVisitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logEqv(final IntIntegerStruct integer2) {
			final long l1 = integer1.l;
			final int i = integer2.i;
			final long xor = l1 ^ i;
			return IntegerStruct.valueOf(~xor);
		}

		@Override
		public IntegerStruct logEqv(final LongIntegerStruct integer2) {
			final long l1 = integer1.l;
			final long l2 = integer2.l;
			final long xor = l1 ^ l2;
			return IntegerStruct.valueOf(~xor);
		}

		@Override
		public IntegerStruct logEqv(final BigIntegerStruct integer2) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;
			final BigInteger xor = lBigInteger.xor(bigInteger);
			return IntegerStruct.valueOf(xor.not());
		}
	}

	/**
	 * {@link IntegerStruct.LogIorVisitor} for computing bitwise inclusive-or results for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerLogIorVisitor extends IntegerStruct.LogIorVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLogIorVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise inclusive-or operation
		 */
		private LongIntegerLogIorVisitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logIor(final IntIntegerStruct integer2) {
			final long l1 = integer1.l;
			final int i = integer2.i;
			return IntegerStruct.valueOf(l1 | i);
		}

		@Override
		public IntegerStruct logIor(final LongIntegerStruct integer2) {
			final long l1 = integer1.l;
			final long l2 = integer2.l;
			return IntegerStruct.valueOf(l1 | l2);
		}

		@Override
		public IntegerStruct logIor(final BigIntegerStruct integer2) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(lBigInteger.or(bigInteger));
		}
	}

	/**
	 * {@link IntegerStruct.LogNandVisitor} for computing bitwise nand results for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerLogNandVisitor extends IntegerStruct.LogNandVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLogNandVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise nand operation
		 */
		private LongIntegerLogNandVisitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logNand(final IntIntegerStruct integer2) {
			final long l1 = integer1.l;
			final int i = integer2.i;
			final long and = l1 & i;
			return IntegerStruct.valueOf(~and);
		}

		@Override
		public IntegerStruct logNand(final LongIntegerStruct integer2) {
			final long l1 = integer1.l;
			final long l2 = integer2.l;
			final long and = l1 & l2;
			return IntegerStruct.valueOf(~and);
		}

		@Override
		public IntegerStruct logNand(final BigIntegerStruct integer2) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;
			final BigInteger and = lBigInteger.and(bigInteger);
			return IntegerStruct.valueOf(and.not());
		}
	}

	/**
	 * {@link IntegerStruct.LogNorVisitor} for computing bitwise inclusive-nor results for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerLogNorVisitor extends IntegerStruct.LogNorVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLogNorVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise inclusive-nor operation
		 */
		private LongIntegerLogNorVisitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logNor(final IntIntegerStruct integer2) {
			final long l1 = integer1.l;
			final int i = integer2.i;
			final long or = l1 | i;
			return IntegerStruct.valueOf(~or);
		}

		@Override
		public IntegerStruct logNor(final LongIntegerStruct integer2) {
			final long l1 = integer1.l;
			final long l2 = integer2.l;
			final long or = l1 | l2;
			return IntegerStruct.valueOf(~or);
		}

		@Override
		public IntegerStruct logNor(final BigIntegerStruct integer2) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;
			final BigInteger or = lBigInteger.or(bigInteger);
			return IntegerStruct.valueOf(or.not());
		}
	}

	/**
	 * {@link IntegerStruct.LogOrC1Visitor} for computing bitwise inclusive-or, with complementary first, results for
	 * {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerLogOrC1Visitor extends IntegerStruct.LogOrC1Visitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLogOrC1Visitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument, to be complementary, in the bitwise inclusive-or operation
		 */
		private LongIntegerLogOrC1Visitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logOrC1(final IntIntegerStruct integer2) {
			final long l1 = integer1.l;
			final int i = integer2.i;
			return IntegerStruct.valueOf(~l1 | i);
		}

		@Override
		public IntegerStruct logOrC1(final LongIntegerStruct integer2) {
			final long l1 = integer1.l;
			final long l2 = integer2.l;
			return IntegerStruct.valueOf(~l1 | l2);
		}

		@Override
		public IntegerStruct logOrC1(final BigIntegerStruct integer2) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(lBigInteger.not().or(bigInteger));
		}
	}

	/**
	 * {@link IntegerStruct.LogOrC2Visitor} for computing bitwise inclusive-or, with complementary second, results for
	 * {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerLogOrC2Visitor extends IntegerStruct.LogOrC2Visitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLogOrC2Visitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise inclusive-or, with complementary second, operation
		 */
		private LongIntegerLogOrC2Visitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logOrC2(final IntIntegerStruct integer2) {
			final long l1 = integer1.l;
			final int i = integer2.i;
			return IntegerStruct.valueOf(l1 | ~i);
		}

		@Override
		public IntegerStruct logOrC2(final LongIntegerStruct integer2) {
			final long l1 = integer1.l;
			final long l2 = integer2.l;
			return IntegerStruct.valueOf(l1 | ~l2);
		}

		@Override
		public IntegerStruct logOrC2(final BigIntegerStruct integer2) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(lBigInteger.or(bigInteger.not()));
		}
	}

	/**
	 * {@link IntegerStruct.LogXorVisitor} for computing bitwise exclusive-or results for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerLogXorVisitor extends IntegerStruct.LogXorVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLogXorVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise exclusive-or operation
		 */
		private LongIntegerLogXorVisitor(final LongIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logXor(final IntIntegerStruct integer2) {
			final long l1 = integer1.l;
			final int i = integer2.i;
			return IntegerStruct.valueOf(l1 ^ i);
		}

		@Override
		public IntegerStruct logXor(final LongIntegerStruct integer2) {
			final long l1 = integer1.l;
			final long l2 = integer2.l;
			return IntegerStruct.valueOf(l1 ^ l2);
		}

		@Override
		public IntegerStruct logXor(final BigIntegerStruct integer2) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer1.l);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(lBigInteger.xor(bigInteger));
		}
	}

	/**
	 * {@link IntegerStruct.LogBitPVisitor} for testing the active bit by index for {@link LongIntegerStruct}s.
	 */
	private static final class LongIntegerLogBitPVisitor extends IntegerStruct.LogBitPVisitor<LongIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an LongIntegerLogBitPVisitor with the provided {@link
		 * LongIntegerStruct}.
		 *
		 * @param integer
		 * 		the {@link LongIntegerStruct} to perform the active bit by index test
		 */
		private LongIntegerLogBitPVisitor(final LongIntegerStruct integer) {
			super(integer);
		}

		@Override
		public boolean logBitP(final IntIntegerStruct index) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer.l);
			final int indexInt = index.intValue();
			return lBigInteger.testBit(indexInt);
		}

		@Override
		public boolean logBitP(final LongIntegerStruct index) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer.l);
			final int indexInt = index.intValue();
			return lBigInteger.testBit(indexInt);
		}

		@Override
		public boolean logBitP(final BigIntegerStruct index) {
			final BigInteger lBigInteger = BigInteger.valueOf(integer.l);
			final int indexInt = index.intValue();
			return lBigInteger.testBit(indexInt);
		}
	}
}

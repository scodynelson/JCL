/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

import jcl.classes.BuiltInClassStruct;
import jcl.types.FixnumType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apache.commons.math3.util.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link IntIntegerStruct} is the object representation of a Lisp 'integer' type that can be contained within a
 * Java {@code int}.
 */
public final class IntIntegerStruct extends BuiltInClassStruct implements IntegerStruct {

	private static final Logger LOGGER = LoggerFactory.getLogger(IntIntegerStruct.class);

	/**
	 * {@link IntIntegerStruct} constant representing 0.
	 */
	public static final IntIntegerStruct ZERO = valueOf(0);

	/**
	 * {@link IntIntegerStruct} constant representing 1.
	 */
	public static final IntIntegerStruct ONE = valueOf(1);

	/**
	 * {@link IntIntegerStruct} constant representing 2.
	 */
	public static final IntIntegerStruct TWO = valueOf(2);

	/**
	 * {@link IntIntegerStruct} constant representing 10.
	 */
	public static final IntIntegerStruct TEN = valueOf(10);

	/**
	 * {@link IntIntegerStruct} constant representing -1.
	 */
	public static final IntIntegerStruct MINUS_ONE = valueOf(-1);

	/**
	 * The internal {@code int} containing the int contents.
	 */
	final int i;

	/**
	 * Private constructor.
	 *
	 * @param i
	 * 		the value of the IntIntegerStruct
	 */
	private IntIntegerStruct(final int i) {
		super(FixnumType.INSTANCE, null, null);
		this.i = i;
	}

	/**
	 * Returns a IntIntegerStruct object with the provided {@code int} value.
	 *
	 * @param i
	 * 		the int value of the resulting IntIntegerStruct
	 *
	 * @return a IntIntegerStruct object with the provided {@code int} value
	 */
	public static IntIntegerStruct valueOf(final int i) {
		return new IntIntegerStruct(i);
	}

	/*
		IntegerStruct
	 */

	@Override
	public int intValue() {
		return i;
	}

	@Override
	public long longValue() {
		return i;
	}

	@Override
	public BigInteger bigIntegerValue() {
		return BigInteger.valueOf(i);
	}

	@Override
	public boolean evenp() {
		return (i % 2) == 0;
	}

	@Override
	public boolean oddp() {
		return (i % 2) != 0;
	}

	@Override
	public IntIntegerStruct isqrt() {
		final double sqrt = StrictMath.sqrt(i);
		final Double floor = StrictMath.floor(sqrt);
		// NOTE: a root can only be less than the original value. Since the original value was an int,
		//          the result will be an int.
		return valueOf(floor.intValue());
	}

	@Override
	public IntegerStruct gcd(final IntegerStruct.GcdVisitor<?> gcdVisitor) {
		return gcdVisitor.gcd(this);
	}

	@Override
	public IntegerStruct.GcdVisitor<?> gcdVisitor() {
		return new IntIntegerGcdVisitor(this);
	}

	@Override
	public IntegerStruct lcm(final IntegerStruct.LcmVisitor<?> lcmVisitor) {
		return lcmVisitor.lcm(this);
	}

	@Override
	public IntegerStruct.LcmVisitor<?> lcmVisitor() {
		return new IntIntegerLcmVisitor(this);
	}

	@Override
	public IntegerStruct ash(final IntegerStruct.AshVisitor<?> ashVisitor) {
		return ashVisitor.ash(this);
	}

	@Override
	public IntegerStruct.AshVisitor<?> ashVisitor() {
		return new IntIntegerAshVisitor(this);
	}

	@Override
	public IntegerStruct logAnd(final IntegerStruct.LogAndVisitor<?> logAndVisitor) {
		return logAndVisitor.logAnd(this);
	}

	@Override
	public IntegerStruct.LogAndVisitor<?> logAndVisitor() {
		return new IntIntegerLogAndVisitor(this);
	}

	@Override
	public IntegerStruct logAndC1(final IntegerStruct.LogAndC1Visitor<?> logAndC1Visitor) {
		return logAndC1Visitor.logAndC1(this);
	}

	@Override
	public IntegerStruct.LogAndC1Visitor<?> logAndC1Visitor() {
		return new IntIntegerLogAndC1Visitor(this);
	}

	@Override
	public IntegerStruct logAndC2(final IntegerStruct.LogAndC2Visitor<?> logAndC2Visitor) {
		return logAndC2Visitor.logAndC2(this);
	}

	@Override
	public IntegerStruct.LogAndC2Visitor<?> logAndC2Visitor() {
		return new IntIntegerLogAndC2Visitor(this);
	}

	@Override
	public IntegerStruct logEqv(final IntegerStruct.LogEqvVisitor<?> logEqvVisitor) {
		return logEqvVisitor.logEqv(this);
	}

	@Override
	public IntegerStruct.LogEqvVisitor<?> logEqvVisitor() {
		return new IntIntegerLogEqvVisitor(this);
	}

	@Override
	public IntegerStruct logIor(final IntegerStruct.LogIorVisitor<?> logIorVisitor) {
		return logIorVisitor.logIor(this);
	}

	@Override
	public IntegerStruct.LogIorVisitor<?> logIorVisitor() {
		return new IntIntegerLogIorVisitor(this);
	}

	@Override
	public IntegerStruct logNand(final IntegerStruct.LogNandVisitor<?> logNandVisitor) {
		return logNandVisitor.logNand(this);
	}

	@Override
	public IntegerStruct.LogNandVisitor<?> logNandVisitor() {
		return new IntIntegerLogNandVisitor(this);
	}

	@Override
	public IntegerStruct logNor(final IntegerStruct.LogNorVisitor<?> logNorVisitor) {
		return logNorVisitor.logNor(this);
	}

	@Override
	public IntegerStruct.LogNorVisitor<?> logNorVisitor() {
		return new IntIntegerLogNorVisitor(this);
	}

	@Override
	public IntIntegerStruct logNot() {
		final int not = ~i;
		return valueOf(not);
	}

	@Override
	public IntegerStruct logOrC1(final IntegerStruct.LogOrC1Visitor<?> logOrC1Visitor) {
		return logOrC1Visitor.logOrC1(this);
	}

	@Override
	public IntegerStruct.LogOrC1Visitor<?> logOrC1Visitor() {
		return new IntIntegerLogOrC1Visitor(this);
	}

	@Override
	public IntegerStruct logOrC2(final IntegerStruct.LogOrC2Visitor<?> logOrC2Visitor) {
		return logOrC2Visitor.logOrC2(this);
	}

	@Override
	public IntegerStruct.LogOrC2Visitor<?> logOrC2Visitor() {
		return new IntIntegerLogOrC2Visitor(this);
	}

	@Override
	public IntegerStruct logXor(final IntegerStruct.LogXorVisitor<?> logXorVisitor) {
		return logXorVisitor.logXor(this);
	}

	@Override
	public IntegerStruct.LogXorVisitor<?> logXorVisitor() {
		return new IntIntegerLogXorVisitor(this);
	}

	@Override
	public boolean logBitP(final IntegerStruct.LogBitPVisitor<?> logBitPVisitor) {
		return logBitPVisitor.logBitP(this);
	}

	@Override
	public IntegerStruct.LogBitPVisitor<?> logBitPVisitor() {
		return new IntIntegerLogBitPVisitor(this);
	}

	@Override
	public IntIntegerStruct logCount() {
		final int bitCount = Integer.bitCount(i);
		return valueOf(bitCount);
	}

	@Override
	public IntegerStruct integerLength() {
		// TODO: is this the best way??
		final double log2 = FastMath.log(i, 2);
		final Double ceil = StrictMath.ceil(log2);
		return IntegerStruct.valueOf(ceil.longValue());
	}

	/*
		RealStruct
	 */

	@Override
	public boolean plusp() {
		return i > 0;
	}

	@Override
	public boolean minusp() {
		return i < 0;
	}

	@Override
	public boolean isLessThan(final RealStruct.LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	@Override
	public RealStruct.LessThanVisitor<?> lessThanVisitor() {
		return new IntegerLessThanVisitor(this);
	}

	@Override
	public boolean isGreaterThan(final RealStruct.GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	@Override
	public RealStruct.GreaterThanVisitor<?> greaterThanVisitor() {
		return new IntegerGreaterThanVisitor(this);
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	@Override
	public RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new IntegerLessThanOrEqualToVisitor(this);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	@Override
	public RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new IntegerGreaterThanOrEqualToVisitor(this);
	}

	@Override
	public QuotientRemainderResult floor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.floor(this);
	}

	@Override
	public QuotientRemainderResult ffloor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ffloor(this);
	}

	@Override
	public QuotientRemainderResult ceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ceiling(this);
	}

	@Override
	public QuotientRemainderResult fceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fceiling(this);
	}

	@Override
	public QuotientRemainderResult round(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.round(this);
	}

	@Override
	public QuotientRemainderResult fround(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fround(this);
	}

	@Override
	public QuotientRemainderVisitor<?> quotientRemainderVisitor() {
		return new IntIntegerQuotientRemainderVisitor(this);
	}

	/*
		NumberStruct
	 */

	@Override
	public RealStruct abs() {
		if (i >= 0) {
			return this;
		}
		return negation();
	}

	@Override
	public boolean zerop() {
		return i == 0;
	}

	@Override
	public NumberStruct add(final AddVisitor<?> addVisitor) {
		return addVisitor.add(this);
	}

	@Override
	public AddVisitor<?> addVisitor() {
		return new IntegerAddVisitor(this);
	}

	@Override
	public NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	public SubtractVisitor<?> subtractVisitor() {
		return new IntegerSubtractVisitor(this);
	}

	@Override
	public NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	public MultiplyVisitor<?> multiplyVisitor() {
		return new IntegerMultiplyVisitor(this);
	}

	@Override
	public NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	public DivideVisitor<?> divideVisitor() {
		return new IntegerDivideVisitor(this);
	}

	@Override
	public boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	@Override
	public EqualToVisitor<?> equalToVisitor() {
		return new IntegerEqualToVisitor(this);
	}

	@Override
	public NumberStruct expt(final ExptVisitor<?> exptVisitor) {
		return exptVisitor.expt(this);
	}

	@Override
	public ExptVisitor<?> exptVisitor() {
		return new IntegerExptVisitor(this);
	}

	@Override
	public IntegerStruct negation() {
		return valueOf(-i);
	}

	@Override
	public NumberStruct reciprocal() {
		return new RatioStruct(BigInteger.ONE, bigIntegerValue());
	}

	// Comparison Visitor Helpers

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
	private static int getComparisonResult(final IntIntegerStruct number1, final IntIntegerStruct number2) {
		final BigInteger bigInteger1 = number1.getBigInteger();
		final BigInteger bigInteger2 = number2.getBigInteger();
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
	private static int getComparisonResult(final IntIntegerStruct number1, final RatioStruct number2) {
		final BigInteger bigInteger1 = number1.getBigInteger();

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
		return new HashCodeBuilder().append(i)
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
		final IntIntegerStruct rhs = (IntIntegerStruct) obj;
		return new EqualsBuilder().append(i, rhs.i)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return String.valueOf(i);
	}

	// Visitor Implementations

	/**
	 * {@link RealStruct.RealAddVisitor} for computing addition results for {@link IntIntegerStruct}s.
	 */
	private static final class IntegerAddVisitor extends RealStruct.RealAddVisitor<IntIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerAddVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		private IntegerAddVisitor(final IntIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct add(final IntIntegerStruct number2) {
			final int i1 = number1.i;
			final int i2 = number2.i;
			return IntegerStruct.valueOf(i1 + i2);
		}

		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			final int i1 = number1.i;
			final long l = number2.l;
			return IntegerStruct.valueOf(i1 + l);
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			final BigInteger iBigInteger = BigInteger.valueOf(number1.i);
			final BigInteger bigInteger = number2.bigInteger;
			final BigInteger add = iBigInteger.add(bigInteger);
			return BigIntegerStruct.valueOf(add);
		}

		@Override
		public RealStruct add(final SingleFloatStruct number2) {
			final int i1 = number1.i;
			final float f = number2.f;
			return FloatStruct.valueOf(i1 + f);
		}

		@Override
		public RealStruct add(final DoubleFloatStruct number2) {
			final int i1 = number1.i;
			final double d = number2.d;
			return FloatStruct.valueOf(i1 + d);
		}

		@Override
		public RealStruct add(final BigFloatStruct number2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(number1.i);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal add = iBigDecimal.add(bigDecimal);
			return BigFloatStruct.valueOf(add);
		}

		@Override
		public RealStruct add(final RatioStruct number2) {
			final BigFraction iBigFraction = new BigFraction(number1.i);
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction add = iBigFraction.add(bigFraction);
			return RationalStruct.makeRational(add);
		}

		@Override
		public NumberStruct add(final ComplexStruct number2) {
			// TODO
			return super.add(number2);
		}
	}

	/**
	 * {@link RealStruct.RealSubtractVisitor} for computing subtraction function results for {@link IntIntegerStruct}s.
	 */
	private static final class IntegerSubtractVisitor extends RealStruct.RealSubtractVisitor<IntIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerSubtractVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		IntegerSubtractVisitor(final IntIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct subtract(final IntIntegerStruct number2) {
			final int i1 = number1.i;
			final int i2 = number2.i;
			return IntegerStruct.valueOf(i1 - i2);
		}

		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			final int i1 = number1.i;
			final long l = number2.l;
			return IntegerStruct.valueOf(i1 - l);
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			final BigInteger iBigInteger = BigInteger.valueOf(number1.i);
			final BigInteger bigInteger = number2.bigInteger;
			final BigInteger subtract = iBigInteger.subtract(bigInteger);
			return IntegerStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final SingleFloatStruct number2) {
			final int i1 = number1.i;
			final float f = number2.f;
			return FloatStruct.valueOf(i1 - f);
		}

		@Override
		public RealStruct subtract(final DoubleFloatStruct number2) {
			final int i1 = number1.i;
			final double d = number2.d;
			return FloatStruct.valueOf(i1 - d);
		}

		@Override
		public RealStruct subtract(final BigFloatStruct number2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(number1.i);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal subtract = iBigDecimal.subtract(bigDecimal);
			return FloatStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final BigFraction iBigFraction = new BigFraction(number1.i);
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction subtract = iBigFraction.subtract(bigFraction);
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
	 * IntIntegerStruct}s.
	 */
	private static final class IntegerMultiplyVisitor extends RealStruct.RealMultiplyVisitor<IntIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerMultiplyVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		IntegerMultiplyVisitor(final IntIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct multiply(final IntIntegerStruct number2) {
			final int i1 = number1.i;
			final int i2 = number2.i;
			return IntegerStruct.valueOf(i1 * i2);
		}

		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			final int i1 = number1.i;
			final long l = number2.l;
			return IntegerStruct.valueOf(i1 * l);
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			final BigInteger iBigInteger = BigInteger.valueOf(number1.i);
			final BigInteger bigInteger = number2.bigInteger;
			final BigInteger multiply = iBigInteger.multiply(bigInteger);
			return BigIntegerStruct.valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final SingleFloatStruct number2) {
			final int i1 = number1.i;
			final float f = number2.f;
			return FloatStruct.valueOf(i1 * f);
		}

		@Override
		public RealStruct multiply(final DoubleFloatStruct number2) {
			final int i1 = number1.i;
			final double d = number2.d;
			return FloatStruct.valueOf(i1 * d);
		}

		@Override
		public RealStruct multiply(final BigFloatStruct number2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(number1.i);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal multiply = iBigDecimal.multiply(bigDecimal);
			return BigFloatStruct.valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final BigFraction iBigFraction = new BigFraction(number1.i);
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction multiply = iBigFraction.multiply(bigFraction);
			return RationalStruct.makeRational(multiply);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number2) {
			// TODO
			return super.multiply(number2);
		}
	}

	/**
	 * {@link RealStruct.RealDivideVisitor} for computing division function results for {@link IntIntegerStruct}s.
	 */
	private static final class IntegerDivideVisitor extends RealStruct.RealDivideVisitor<IntIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerDivideVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		IntegerDivideVisitor(final IntIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct divide(final IntIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.bigIntegerValue();
			final BigInteger bigInteger2 = number2.bigIntegerValue();
			return RationalStruct.makeRational(bigInteger1, bigInteger2);
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.bigIntegerValue();
			final BigInteger bigInteger2 = number2.bigIntegerValue();
			return RationalStruct.makeRational(bigInteger1, bigInteger2);
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.bigIntegerValue();
			final BigInteger bigInteger2 = number2.bigIntegerValue();
			return RationalStruct.makeRational(bigInteger1, bigInteger2);
		}

		@Override
		public RealStruct divide(final SingleFloatStruct number2) {
			final int i1 = number1.i;
			final float f = number2.f;
			return FloatStruct.valueOf(i1 / f);
		}

		@Override
		public RealStruct divide(final DoubleFloatStruct number2) {
			final int i1 = number1.i;
			final double d = number2.d;
			return FloatStruct.valueOf(i1 / d);
		}

		@Override
		public RealStruct divide(final BigFloatStruct number2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(number1.i);
			final BigDecimal bigDecimal = number2.bigDecimal;
			final BigDecimal divide = iBigDecimal.divide(bigDecimal, MathContext.DECIMAL128);
			return FloatStruct.valueOf(divide);
		}

		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigFraction iBigFraction = new BigFraction(number1.i);
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction divide = iBigFraction.divide(bigFraction);
			return RationalStruct.makeRational(divide);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			// TODO
			return super.divide(number2);
		}
	}

	/**
	 * {@link RealStruct.RealEqualToVisitor} for computing numeric '=' equality results for {@link IntIntegerStruct}s.
	 */
	private static final class IntegerEqualToVisitor extends RealStruct.RealEqualToVisitor<IntIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerEqualToVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		IntegerEqualToVisitor(final IntIntegerStruct number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final IntIntegerStruct number2) {
			return number1.i == number2.i;
		}

		@Override
		public boolean equalTo(final LongIntegerStruct number2) {
			return number1.i == number2.l;
		}

		@Override
		public boolean equalTo(final BigIntegerStruct number2) {
			final BigInteger iBigInteger = BigInteger.valueOf(number1.i);
			final BigInteger bigInteger = number2.bigInteger;
			return iBigInteger.compareTo(bigInteger) == 0;
		}

		@Override
		public boolean equalTo(final SingleFloatStruct number2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(number1.i);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			return iBigDecimal.compareTo(bigDecimal) == 0;
		}

		@Override
		public boolean equalTo(final DoubleFloatStruct number2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(number1.i);
			final BigDecimal bigDecimal = number2.bigDecimalValue();
			return iBigDecimal.compareTo(bigDecimal) == 0;
		}

		@Override
		public boolean equalTo(final BigFloatStruct number2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(number1.i);
			final BigDecimal bigDecimal = number2.bigDecimal;
			return iBigDecimal.compareTo(bigDecimal) == 0;
		}

		@Override
		public boolean equalTo(final RatioStruct number2) {
			final BigFraction iBigFraction = new BigFraction(number1.i);
			final BigFraction bigFraction = number2.bigFraction;
			return iBigFraction.equals(bigFraction);
		}
	}

	/**
	 * {@link RealStruct.LessThanVisitor} for computing numeric {@literal '<'} equality results for {@link
	 * IntIntegerStruct}s.
	 */
	private static final class IntegerLessThanVisitor extends RealStruct.LessThanVisitor<IntIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerLessThanVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		IntegerLessThanVisitor(final IntIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThan(final IntIntegerStruct real2) {
			return real1.i < real2.i;
		}

		@Override
		public boolean lessThan(final LongIntegerStruct real2) {
			return real1.i < real2.l;
		}

		@Override
		public boolean lessThan(final BigIntegerStruct real2) {
			final BigInteger iBigInteger = BigInteger.valueOf(real1.i);
			final BigInteger bigInteger = real2.bigInteger;
			return iBigInteger.compareTo(bigInteger) < 0;
		}

		@Override
		public boolean lessThan(final SingleFloatStruct real2) {
			return real1.i < real2.f;
		}

		@Override
		public boolean lessThan(final DoubleFloatStruct real2) {
			return real1.i < real2.d;
		}

		@Override
		public boolean lessThan(final BigFloatStruct real2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(real1.i);
			final BigDecimal bigDecimal = real2.bigDecimal;
			return iBigDecimal.compareTo(bigDecimal) < 0;
		}

		@Override
		public boolean lessThan(final RatioStruct real2) {
			final BigFraction iBigFraction = new BigFraction(real1.i);
			final BigFraction bigFraction = real2.bigFraction;
			return iBigFraction.compareTo(bigFraction) < 0;
		}
	}

	/**
	 * {@link RealStruct.GreaterThanVisitor} for computing numeric {@literal '>'} equality results for {@link
	 * IntIntegerStruct}s.
	 */
	private static final class IntegerGreaterThanVisitor extends RealStruct.GreaterThanVisitor<IntIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerGreaterThanVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		IntegerGreaterThanVisitor(final IntIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThan(final IntIntegerStruct real2) {
			return real1.i > real2.i;
		}

		@Override
		public boolean greaterThan(final LongIntegerStruct real2) {
			return real1.i > real2.l;
		}

		@Override
		public boolean greaterThan(final BigIntegerStruct real2) {
			final BigInteger iBigInteger = BigInteger.valueOf(real1.i);
			final BigInteger bigInteger = real2.bigInteger;
			return iBigInteger.compareTo(bigInteger) > 0;
		}

		@Override
		public boolean greaterThan(final SingleFloatStruct real2) {
			return real1.i > real2.f;
		}

		@Override
		public boolean greaterThan(final DoubleFloatStruct real2) {
			return real1.i > real2.d;
		}

		@Override
		public boolean greaterThan(final BigFloatStruct real2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(real1.i);
			final BigDecimal bigDecimal = real2.bigDecimal;
			return iBigDecimal.compareTo(bigDecimal) > 0;
		}

		@Override
		public boolean greaterThan(final RatioStruct real2) {
			final BigFraction iBigFraction = new BigFraction(real1.i);
			final BigFraction bigFraction = real2.bigFraction;
			return iBigFraction.compareTo(bigFraction) > 0;
		}
	}

	/**
	 * {@link RealStruct.LessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality results for {@link
	 * IntIntegerStruct}s.
	 */
	private static final class IntegerLessThanOrEqualToVisitor extends RealStruct.LessThanOrEqualToVisitor<IntIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerLessThanOrEqualToVisitor with the provided
		 * {@link IntIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		IntegerLessThanOrEqualToVisitor(final IntIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThanOrEqualTo(final IntIntegerStruct real2) {
			return real1.i <= real2.i;
		}

		@Override
		public boolean lessThanOrEqualTo(final LongIntegerStruct real2) {
			return real1.i <= real2.l;
		}

		@Override
		public boolean lessThanOrEqualTo(final BigIntegerStruct real2) {
			final BigInteger iBigInteger = BigInteger.valueOf(real1.i);
			final BigInteger bigInteger = real2.bigInteger;
			return iBigInteger.compareTo(bigInteger) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final SingleFloatStruct real2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(real1.i);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return iBigDecimal.compareTo(bigDecimal) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final DoubleFloatStruct real2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(real1.i);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return iBigDecimal.compareTo(bigDecimal) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final BigFloatStruct real2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(real1.i);
			final BigDecimal bigDecimal = real2.bigDecimal;
			return iBigDecimal.compareTo(bigDecimal) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			final BigFraction iBigFraction = new BigFraction(real1.i);
			final BigFraction bigFraction = real2.bigFraction;
			return iBigFraction.compareTo(bigFraction) <= 0;
		}
	}

	/**
	 * {@link RealStruct.GreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality results for {@link
	 * IntIntegerStruct}s.
	 */
	private static final class IntegerGreaterThanOrEqualToVisitor extends RealStruct.GreaterThanOrEqualToVisitor<IntIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerGreaterThanOrEqualToVisitor with the
		 * provided {@link IntIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		IntegerGreaterThanOrEqualToVisitor(final IntIntegerStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThanOrEqualTo(final IntIntegerStruct real2) {
			return real1.i >= real2.i;
		}

		@Override
		public boolean greaterThanOrEqualTo(final LongIntegerStruct real2) {
			return real1.i >= real2.l;
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigIntegerStruct real2) {
			final BigInteger iBigInteger = BigInteger.valueOf(real1.i);
			final BigInteger bigInteger = real2.bigInteger;
			return iBigInteger.compareTo(bigInteger) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final SingleFloatStruct real2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(real1.i);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return iBigDecimal.compareTo(bigDecimal) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final DoubleFloatStruct real2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(real1.i);
			final BigDecimal bigDecimal = real2.bigDecimalValue();
			return iBigDecimal.compareTo(bigDecimal) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigFloatStruct real2) {
			final BigDecimal iBigDecimal = BigDecimal.valueOf(real1.i);
			final BigDecimal bigDecimal = real2.bigDecimal;
			return iBigDecimal.compareTo(bigDecimal) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			final BigFraction iBigFraction = new BigFraction(real1.i);
			final BigFraction bigFraction = real2.bigFraction;
			return iBigFraction.compareTo(bigFraction) >= 0;
		}
	}

	/**
	 * {@link IntIntegerQuotientRemainderVisitor} for computing quotient and remainder results for {@link
	 * IntIntegerStruct}s.
	 */
	private static final class IntIntegerQuotientRemainderVisitor extends RationalStruct.RationalQuotientRemainderVisitor<IntIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerQuotientRemainderVisitor with the provided
		 * {@link IntIntegerStruct}.
		 *
		 * @param real
		 * 		the real argument in the computational quotient and remainder operation
		 */
		IntIntegerQuotientRemainderVisitor(final IntIntegerStruct real) {
			super(real);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final IntIntegerStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {

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
		public QuotientRemainderResult quotientRemainder(final LongIntegerStruct divisor, final RoundingMode roundingMode, final boolean isQuotientFloat) {
			return null;
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final BigIntegerStruct divisor, final RoundingMode roundingMode, final boolean isQuotientFloat) {
			return null;
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final RatioStruct divisor, final RoundingMode roundingMode, final boolean isQuotientFloat) {
			return super.quotientRemainder(divisor, roundingMode, isQuotientFloat);
		}
	}

	/**
	 * {@link RealStruct.RealExptVisitor} for computing exponential function results for {@link IntIntegerStruct}s.
	 */
	private static final class IntegerExptVisitor extends RealStruct.RealExptVisitor<IntIntegerStruct> {
		// TODO: Do all this...

		/**
		 * Package private constructor to make a new instance of an IntegerExptVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param base
		 * 		the base argument in the exponential operation
		 */
		IntegerExptVisitor(final IntIntegerStruct base) {
			super(base);
		}

		@Override
		public NumberStruct expt(final IntIntegerStruct power) {
			if (power.minusp()) {
				return exptInteger(base, power);
			} else {
				final BigInteger baseBigInteger = base.getBigInteger();
				final BigInteger powerBigInteger = power.getBigInteger();
				final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, powerBigInteger);
				return IntegerStruct.valueOf(pow);
			}
		}

		@Override
		public NumberStruct expt(final LongIntegerStruct power) {
			return super.expt(power);
		}

		@Override
		public NumberStruct expt(final BigIntegerStruct power) {
			return super.expt(power);
		}

		@Override
		public NumberStruct expt(final SingleFloatStruct power) {
			return super.expt(power);
		}

		@Override
		public NumberStruct expt(final DoubleFloatStruct power) {
			return super.expt(power);
		}

		@Override
		public NumberStruct expt(final BigFloatStruct power) {
			return super.expt(power);
		}

		@Override
		public NumberStruct expt(final RatioStruct power) {
			return super.expt(power);
		}

		@Override
		public NumberStruct expt(final ComplexStruct power) {
			return super.expt(power);
		}
	}

	private static final class IntIntegerGcdVisitor extends IntegerStruct.GcdVisitor<IntIntegerStruct> {

		IntIntegerGcdVisitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct gcd(final IntIntegerStruct integer2) {
			final int gcd = ArithmeticUtils.gcd(integer1.i, integer2.i);
			return valueOf(gcd);
		}

		@Override
		public IntegerStruct gcd(final LongIntegerStruct integer2) {
			final long gcd = ArithmeticUtils.gcd(integer1.i, integer2.l);
			return IntegerStruct.valueOf(gcd);
		}

		@Override
		public IntegerStruct gcd(final BigIntegerStruct integer2) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;
			final BigInteger gcd = iBigInteger.gcd(bigInteger);
			return IntegerStruct.valueOf(gcd);
		}
	}

	private static final class IntIntegerLcmVisitor extends IntegerStruct.LcmVisitor<IntIntegerStruct> {

		IntIntegerLcmVisitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct lcm(final IntIntegerStruct integer2) {
			final int lcm = ArithmeticUtils.lcm(integer1.i, integer2.i);
			return valueOf(lcm);
		}

		@Override
		public IntegerStruct lcm(final LongIntegerStruct integer2) {
			final long lcm = ArithmeticUtils.lcm(integer1.i, integer2.l);
			return IntegerStruct.valueOf(lcm);
		}

		@Override
		public IntegerStruct lcm(final BigIntegerStruct integer2) {
			if (integer1.zerop() || integer2.zerop()) {
				return ZERO;
			}

			// lcm(x y) = abs(x * y) / gcd(x y)
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;

			final BigInteger multiply = iBigInteger.multiply(bigInteger);
			final BigInteger abs = multiply.abs();
			final BigInteger gcd = iBigInteger.gcd(bigInteger);
			final BigInteger divide = abs.divide(gcd);

			return IntegerStruct.valueOf(divide);
		}
	}

	private static final class IntIntegerAshVisitor extends IntegerStruct.AshVisitor<IntIntegerStruct> {

		IntIntegerAshVisitor(final IntIntegerStruct integer) {
			super(integer);
		}

		@Override
		public IntIntegerStruct ash(final IntIntegerStruct count) {
			if (count.zerop()) {
				return integer;
			}
			final int countI = count.i;

			final int shiftedI;
			if (countI > 0) {
				// Shift Left if Count is Positive
				shiftedI = integer.i << countI;
			} else {
				// Shift Right if Count is Negative
				shiftedI = integer.i >> countI;
			}
			return valueOf(shiftedI);
		}

		@Override
		public IntIntegerStruct ash(final LongIntegerStruct count) {
			if (count.zerop()) {
				return integer;
			}
			final long countL = count.l;

			final int shiftedI;
			if (countL > 0L) {
				// Shift Left if Count is Positive
				shiftedI = integer.i << countL;
			} else {
				// Shift Right if Count is Negative
				shiftedI = integer.i >> countL;
			}
			return valueOf(shiftedI);
		}

		@Override
		public IntIntegerStruct ash(final BigIntegerStruct count) {
			if (count.zerop()) {
				return integer;
			}
			final BigInteger countBigInteger = count.bigInteger;

			long countL;
			try {
				countL = countBigInteger.longValueExact();
			} catch (final ArithmeticException ignore) {
				LOGGER.warn("Forcibly migrated {} to a long for bit-shifting.", countBigInteger);
				countL = countBigInteger.longValue();
			}

			final int shiftedI;
			if (countL > 0L) {
				// Shift Left if Count is Positive
				shiftedI = integer.i << countL;
			} else {
				// Shift Right if Count is Negative
				shiftedI = integer.i >> countL;
			}
			return valueOf(shiftedI);
		}
	}

	private static final class IntIntegerLogAndVisitor extends IntegerStruct.LogAndVisitor<IntIntegerStruct> {

		IntIntegerLogAndVisitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAnd(final IntIntegerStruct integer2) {
			final int i1 = integer1.i;
			final int i2 = integer2.i;
			return valueOf(i1 & i2);
		}

		@Override
		public IntegerStruct logAnd(final LongIntegerStruct integer2) {
			final int i1 = integer1.i;
			final long l = integer2.l;
			return IntegerStruct.valueOf(i1 & l);
		}

		@Override
		public IntegerStruct logAnd(final BigIntegerStruct integer2) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(iBigInteger.and(bigInteger));
		}
	}

	private static final class IntIntegerLogAndC1Visitor extends IntegerStruct.LogAndC1Visitor<IntIntegerStruct> {

		IntIntegerLogAndC1Visitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAndC1(final IntIntegerStruct integer2) {
			final int i1 = integer1.i;
			final int i2 = integer2.i;
			return valueOf(~i1 & i2);
		}

		@Override
		public IntegerStruct logAndC1(final LongIntegerStruct integer2) {
			final int i1 = integer1.i;
			final long l = integer2.l;
			return IntegerStruct.valueOf(~i1 & l);
		}

		@Override
		public IntegerStruct logAndC1(final BigIntegerStruct integer2) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(iBigInteger.not().and(bigInteger));
		}
	}

	private static final class IntIntegerLogAndC2Visitor extends IntegerStruct.LogAndC2Visitor<IntIntegerStruct> {

		IntIntegerLogAndC2Visitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logAndC2(final IntIntegerStruct integer2) {
			final int i1 = integer1.i;
			final int i2 = integer2.i;
			return valueOf(i1 & ~i2);
		}

		@Override
		public IntegerStruct logAndC2(final LongIntegerStruct integer2) {
			final int i1 = integer1.i;
			final long l = integer2.l;
			return IntegerStruct.valueOf(i1 & ~l);
		}

		@Override
		public IntegerStruct logAndC2(final BigIntegerStruct integer2) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(iBigInteger.and(bigInteger.not()));
		}
	}

	private static final class IntIntegerLogEqvVisitor extends IntegerStruct.LogEqvVisitor<IntIntegerStruct> {

		IntIntegerLogEqvVisitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logEqv(final IntIntegerStruct integer2) {
			final int i1 = integer1.i;
			final int i2 = integer2.i;
			final int xor = i1 ^ i2;
			return valueOf(~xor);
		}

		@Override
		public IntegerStruct logEqv(final LongIntegerStruct integer2) {
			final int i1 = integer1.i;
			final long l = integer2.l;
			final long xor = i1 ^ l;
			return IntegerStruct.valueOf(~xor);
		}

		@Override
		public IntegerStruct logEqv(final BigIntegerStruct integer2) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;
			final BigInteger xor = iBigInteger.xor(bigInteger);
			return IntegerStruct.valueOf(xor.not());
		}
	}

	private static final class IntIntegerLogIorVisitor extends IntegerStruct.LogIorVisitor<IntIntegerStruct> {

		IntIntegerLogIorVisitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logIor(final IntIntegerStruct integer2) {
			final int i1 = integer1.i;
			final int i2 = integer2.i;
			return valueOf(i1 | i2);
		}

		@Override
		public IntegerStruct logIor(final LongIntegerStruct integer2) {
			final int i1 = integer1.i;
			final long l = integer2.l;
			return IntegerStruct.valueOf(i1 | l);
		}

		@Override
		public IntegerStruct logIor(final BigIntegerStruct integer2) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(iBigInteger.or(bigInteger));
		}
	}

	private static final class IntIntegerLogNandVisitor extends IntegerStruct.LogNandVisitor<IntIntegerStruct> {

		IntIntegerLogNandVisitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logNand(final IntIntegerStruct integer2) {
			final int i1 = integer1.i;
			final int i2 = integer2.i;
			final int and = i1 & i2;
			return valueOf(~and);
		}

		@Override
		public IntegerStruct logNand(final LongIntegerStruct integer2) {
			final int i1 = integer1.i;
			final long l = integer2.l;
			final long and = i1 & l;
			return IntegerStruct.valueOf(~and);
		}

		@Override
		public IntegerStruct logNand(final BigIntegerStruct integer2) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;
			final BigInteger and = iBigInteger.and(bigInteger);
			return IntegerStruct.valueOf(and.not());
		}
	}

	private static final class IntIntegerLogNorVisitor extends IntegerStruct.LogNorVisitor<IntIntegerStruct> {

		IntIntegerLogNorVisitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logNor(final IntIntegerStruct integer2) {
			final int i1 = integer1.i;
			final int i2 = integer2.i;
			final int or = i1 | i2;
			return valueOf(~or);
		}

		@Override
		public IntegerStruct logNor(final LongIntegerStruct integer2) {
			final int i1 = integer1.i;
			final long l = integer2.l;
			final long or = i1 | l;
			return IntegerStruct.valueOf(~or);
		}

		@Override
		public IntegerStruct logNor(final BigIntegerStruct integer2) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;
			final BigInteger or = iBigInteger.or(bigInteger);
			return IntegerStruct.valueOf(or.not());
		}
	}

	private static final class IntIntegerLogOrC1Visitor extends IntegerStruct.LogOrC1Visitor<IntIntegerStruct> {

		IntIntegerLogOrC1Visitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logOrC1(final IntIntegerStruct integer2) {
			final int i1 = integer1.i;
			final int i2 = integer2.i;
			return valueOf(~i1 | i2);
		}

		@Override
		public IntegerStruct logOrC1(final LongIntegerStruct integer2) {
			final int i1 = integer1.i;
			final long l = integer2.l;
			return IntegerStruct.valueOf(~i1 | l);
		}

		@Override
		public IntegerStruct logOrC1(final BigIntegerStruct integer2) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(iBigInteger.not().or(bigInteger));
		}
	}

	private static final class IntIntegerLogOrC2Visitor extends IntegerStruct.LogOrC2Visitor<IntIntegerStruct> {

		IntIntegerLogOrC2Visitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logOrC2(final IntIntegerStruct integer2) {
			final int i1 = integer1.i;
			final int i2 = integer2.i;
			return valueOf(i1 | ~i2);
		}

		@Override
		public IntegerStruct logOrC2(final LongIntegerStruct integer2) {
			final int i1 = integer1.i;
			final long l = integer2.l;
			return IntegerStruct.valueOf(i1 | ~l);
		}

		@Override
		public IntegerStruct logOrC2(final BigIntegerStruct integer2) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(iBigInteger.or(bigInteger.not()));
		}
	}

	private static final class IntIntegerLogXorVisitor extends IntegerStruct.LogXorVisitor<IntIntegerStruct> {

		IntIntegerLogXorVisitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct logXor(final IntIntegerStruct integer2) {
			final int i1 = integer1.i;
			final int i2 = integer2.i;
			return valueOf(i1 ^ i2);
		}

		@Override
		public IntegerStruct logXor(final LongIntegerStruct integer2) {
			final int i1 = integer1.i;
			final long l = integer2.l;
			return IntegerStruct.valueOf(i1 ^ l);
		}

		@Override
		public IntegerStruct logXor(final BigIntegerStruct integer2) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger = integer2.bigInteger;
			return IntegerStruct.valueOf(iBigInteger.xor(bigInteger));
		}
	}

	private static final class IntIntegerLogBitPVisitor extends IntegerStruct.LogBitPVisitor<IntIntegerStruct> {

		IntIntegerLogBitPVisitor(final IntIntegerStruct integer) {
			super(integer);
		}

		@Override
		public boolean logBitP(final IntIntegerStruct index) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer.i);
			final int indexInt = index.intValue();
			return iBigInteger.testBit(indexInt);
		}

		@Override
		public boolean logBitP(final LongIntegerStruct index) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer.i);
			final int indexInt = index.intValue();
			return iBigInteger.testBit(indexInt);
		}

		@Override
		public boolean logBitP(final BigIntegerStruct index) {
			final BigInteger iBigInteger = BigInteger.valueOf(integer.i);
			final int indexInt = index.intValue();
			return iBigInteger.testBit(indexInt);
		}
	}
}

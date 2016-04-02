/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.stream.IntStream;

import com.google.common.math.IntMath;
import jcl.classes.BuiltInClassStruct;
import jcl.conditions.exceptions.DivisionByZeroException;
import jcl.types.FixnumType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.exception.MathArithmeticException;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link IntIntegerStruct} is the object representation of a Lisp 'integer' type that can be contained within a
 * Java {@code int}.
 */
public final class IntIntegerStruct extends BuiltInClassStruct implements IntegerStruct {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(IntIntegerStruct.class);

	/**
	 * IntIntegerStruct cache array size.
	 */
	private static final int INT_INTEGER_STRUCT_CACHE_SIZE = 256;

	/**
	 * {@link IntIntegerAddVisitor} cache array size.
	 */
	private static final int INT_INTEGER_STRUCT_ADD_CACHE_SIZE = 50;

	/**
	 * {@link IntIntegerSubtractVisitor} cache array size.
	 */
	private static final int INT_INTEGER_STRUCT_SUBTRACT_CACHE_SIZE = 20;

	/**
	 * {@link IntIntegerMultiplyVisitor} cache array size.
	 */
	private static final int INT_INTEGER_STRUCT_MULTIPLY_CACHE_SIZE = 50;

	/**
	 * {@link IntIntegerDivideVisitor} cache array size.
	 */
	private static final int INT_INTEGER_STRUCT_DIVIDE_CACHE_SIZE = 20;

	/**
	 * IntIntegerStruct cache for reducing object creation.
	 */
	private static final IntIntegerStruct[] INT_INTEGER_STRUCT_CACHE = new IntIntegerStruct[INT_INTEGER_STRUCT_CACHE_SIZE];

	/**
	 * {@link IntIntegerAddVisitor} cache for reducing object creation.
	 */
	private static final IntIntegerAddVisitor[] INT_INTEGER_ADD_CACHE = new IntIntegerAddVisitor[INT_INTEGER_STRUCT_ADD_CACHE_SIZE];

	/**
	 * {@link IntIntegerSubtractVisitor} cache for reducing object creation.
	 */
	private static final IntIntegerSubtractVisitor[] INT_INTEGER_SUBTRACT_CACHE = new IntIntegerSubtractVisitor[INT_INTEGER_STRUCT_SUBTRACT_CACHE_SIZE];

	/**
	 * {@link IntIntegerMultiplyVisitor} cache for reducing object creation.
	 */
	private static final IntIntegerMultiplyVisitor[] INT_INTEGER_MULTIPLY_CACHE = new IntIntegerMultiplyVisitor[INT_INTEGER_STRUCT_MULTIPLY_CACHE_SIZE];

	/**
	 * {@link IntIntegerDivideVisitor} cache for reducing object creation.
	 */
	private static final IntIntegerDivideVisitor[] INT_INTEGER_DIVIDE_CACHE = new IntIntegerDivideVisitor[INT_INTEGER_STRUCT_DIVIDE_CACHE_SIZE];

	static {
		IntStream.range(0, INT_INTEGER_STRUCT_CACHE_SIZE).forEach(value -> {
			final IntIntegerStruct struct = new IntIntegerStruct(value);
			INT_INTEGER_STRUCT_CACHE[value] = struct;
		});
		IntStream.range(0, INT_INTEGER_STRUCT_ADD_CACHE_SIZE).forEach(value -> {
			final IntIntegerStruct struct = INT_INTEGER_STRUCT_CACHE[value];
			final IntIntegerAddVisitor visitor = new IntIntegerAddVisitor(struct);
			INT_INTEGER_ADD_CACHE[value] = visitor;
		});
		IntStream.range(0, INT_INTEGER_STRUCT_SUBTRACT_CACHE_SIZE).forEach(value -> {
			final IntIntegerStruct struct = INT_INTEGER_STRUCT_CACHE[value];
			final IntIntegerSubtractVisitor visitor = new IntIntegerSubtractVisitor(struct);
			INT_INTEGER_SUBTRACT_CACHE[value] = visitor;
		});
		IntStream.range(0, INT_INTEGER_STRUCT_MULTIPLY_CACHE_SIZE).forEach(value -> {
			final IntIntegerStruct struct = INT_INTEGER_STRUCT_CACHE[value];
			final IntIntegerMultiplyVisitor visitor = new IntIntegerMultiplyVisitor(struct);
			INT_INTEGER_MULTIPLY_CACHE[value] = visitor;
		});
		IntStream.range(0, INT_INTEGER_STRUCT_DIVIDE_CACHE_SIZE).forEach(value -> {
			final IntIntegerStruct struct = INT_INTEGER_STRUCT_CACHE[value];
			final IntIntegerDivideVisitor visitor = new IntIntegerDivideVisitor(struct);
			INT_INTEGER_DIVIDE_CACHE[value] = visitor;
		});
	}

	/**
	 * The internal {@code int} containing the IntIntegerStruct contents.
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
	 * 		the {@code int} value of the resulting IntIntegerStruct
	 *
	 * @return a IntIntegerStruct object with the provided {@code int} value
	 */
	public static IntIntegerStruct valueOf(final int i) {
		if ((i >= 0) && (i < INT_INTEGER_STRUCT_CACHE_SIZE)) {
			return INT_INTEGER_STRUCT_CACHE[i];
		}
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
	public IntegerStruct logNot() {
		return IntegerStruct.valueOf(~i);
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
	public IntegerStruct logCount() {
		final int bitCount = Integer.bitCount(i);
		return valueOf(bitCount);
	}

	@Override
	public IntegerStruct integerLength() {
		final int correctedI = (i < 0) ? -i : (i + 1);
		final int bitLength = IntMath.log2(correctedI, RoundingMode.CEILING);
		return valueOf(bitLength);
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
	public IntegerStruct isqrt() {
		final int sqrtFloor = IntMath.sqrt(i, RoundingMode.FLOOR);
		return valueOf(sqrtFloor);
	}

	/*
		RealStruct
	 */

	@Override
	public FloatStruct floatingPoint() {
		return SingleFloatStruct.valueOf(i);
	}

	@Override
	public FloatStruct floatingPoint(final FloatingPointVisitor<?> floatingPointVisitor) {
		return floatingPointVisitor.floatingPoint(this);
	}

	@Override
	public boolean isLessThan(final RealStruct.LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	@Override
	public RealStruct.LessThanVisitor<?> lessThanVisitor() {
		return new IntIntegerLessThanVisitor(this);
	}

	@Override
	public boolean isGreaterThan(final RealStruct.GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	@Override
	public RealStruct.GreaterThanVisitor<?> greaterThanVisitor() {
		return new IntIntegerGreaterThanVisitor(this);
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	@Override
	public RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new IntIntegerLessThanOrEqualToVisitor(this);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	@Override
	public RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new IntIntegerGreaterThanOrEqualToVisitor(this);
	}

	@Override
	public boolean plusp() {
		return i > 0;
	}

	@Override
	public boolean minusp() {
		return i < 0;
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
		if ((i >= 0) && (i < INT_INTEGER_STRUCT_ADD_CACHE_SIZE)) {
			return INT_INTEGER_ADD_CACHE[i];
		}
		return new IntIntegerAddVisitor(this);
	}

	@Override
	public NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	public SubtractVisitor<?> subtractVisitor() {
		if ((i >= 0) && (i < INT_INTEGER_STRUCT_SUBTRACT_CACHE_SIZE)) {
			return INT_INTEGER_SUBTRACT_CACHE[i];
		}
		return new IntIntegerSubtractVisitor(this);
	}

	@Override
	public NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	public MultiplyVisitor<?> multiplyVisitor() {
		if ((i >= 0) && (i < INT_INTEGER_STRUCT_MULTIPLY_CACHE_SIZE)) {
			return INT_INTEGER_MULTIPLY_CACHE[i];
		}
		return new IntIntegerMultiplyVisitor(this);
	}

	@Override
	public NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	public DivideVisitor<?> divideVisitor() {
		if ((i >= 0) && (i < INT_INTEGER_STRUCT_DIVIDE_CACHE_SIZE)) {
			return INT_INTEGER_DIVIDE_CACHE[i];
		}
		return new IntIntegerDivideVisitor(this);
	}

	@Override
	public boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	@Override
	public EqualToVisitor<?> equalToVisitor() {
		return new IntIntegerEqualToVisitor(this);
	}

	@Override
	public NumberStruct expt(final ExptVisitor<?> exptVisitor) {
		return exptVisitor.expt(this);
	}

	@Override
	public ExptVisitor<?> exptVisitor() {
		return new IntIntegerExptVisitor(this);
	}

	@Override
	public boolean zerop() {
		return i == 0;
	}

	@Override
	public RealStruct abs() {
		if (i >= 0) {
			return this;
		}
		return negation();
	}

	@Override
	public IntegerStruct negation() {
		return valueOf(-i);
	}

	@Override
	public NumberStruct reciprocal() {
		if (i == 0) {
			throw new DivisionByZeroException("Division by zero.");
		}
		if (i == 1) {
			return this;
		}
		return RatioStruct.valueOf(BigInteger.ONE, BigInteger.valueOf(i));
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
	private static final class IntIntegerAddVisitor extends RealStruct.RealAddVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerAddVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		private IntIntegerAddVisitor(final IntIntegerStruct number1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(number1.i);
			final BigInteger bigInteger2 = number2.bigInteger;
			final BigInteger add = bigInteger1.add(bigInteger2);
			return BigIntegerStruct.valueOf(add);
		}

		@Override
		public RealStruct add(final SingleFloatStruct number2) {
			final int i1 = number1.i;
			final float f = number2.f;
			return SingleFloatStruct.valueOf(i1 + f);
		}

		@Override
		public RealStruct add(final DoubleFloatStruct number2) {
			final int i1 = number1.i;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(i1 + d);
		}

		@Override
		public RealStruct add(final RatioStruct number2) {
			final int i1 = number1.i;
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction add = bigFraction.add(i1);
			return RationalStruct.valueOf(add);
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
	private static final class IntIntegerSubtractVisitor extends RealStruct.RealSubtractVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerSubtractVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		private IntIntegerSubtractVisitor(final IntIntegerStruct number1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(number1.i);
			final BigInteger bigInteger2 = number2.bigInteger;
			final BigInteger subtract = bigInteger1.subtract(bigInteger2);
			return IntegerStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final SingleFloatStruct number2) {
			final int i1 = number1.i;
			final float f = number2.f;
			return SingleFloatStruct.valueOf(i1 - f);
		}

		@Override
		public RealStruct subtract(final DoubleFloatStruct number2) {
			final int i1 = number1.i;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(i1 - d);
		}

		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final BigFraction bigFraction1 = new BigFraction(number1.i);
			final BigFraction bigFraction2 = number2.bigFraction;
			final BigFraction subtract = bigFraction1.subtract(bigFraction2);
			return RationalStruct.valueOf(subtract);
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
	private static final class IntIntegerMultiplyVisitor extends RealStruct.RealMultiplyVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerMultiplyVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		private IntIntegerMultiplyVisitor(final IntIntegerStruct number1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(number1.i);
			final BigInteger bigInteger2 = number2.bigInteger;
			final BigInteger multiply = bigInteger1.multiply(bigInteger2);
			return BigIntegerStruct.valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final SingleFloatStruct number2) {
			final int i1 = number1.i;
			final float f = number2.f;
			return SingleFloatStruct.valueOf(i1 * f);
		}

		@Override
		public RealStruct multiply(final DoubleFloatStruct number2) {
			final int i1 = number1.i;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(i1 * d);
		}

		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final int i1 = number1.i;
			final BigFraction bigFraction = number2.bigFraction;
			final BigFraction multiply = bigFraction.multiply(i1);
			return RationalStruct.valueOf(multiply);
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
	private static final class IntIntegerDivideVisitor extends RealStruct.RealDivideVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerDivideVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		private IntIntegerDivideVisitor(final IntIntegerStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct divide(final IntIntegerStruct number2) {
			final int i1 = number1.i;
			final int i2 = number2.i;
			return RationalStruct.valueOf(i1, i2);
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			final long l1 = number1.i;
			final long l2 = number2.l;
			return RationalStruct.valueOf(l1, l2);
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			final BigInteger bigInteger1 = BigInteger.valueOf(number1.i);
			final BigInteger bigInteger2 = number2.bigInteger;
			return RationalStruct.valueOf(bigInteger1, bigInteger2);
		}

		@Override
		public RealStruct divide(final SingleFloatStruct number2) {
			final int i1 = number1.i;
			final float f = number2.f;
			return SingleFloatStruct.valueOf(i1 / f);
		}

		@Override
		public RealStruct divide(final DoubleFloatStruct number2) {
			final int i1 = number1.i;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(i1 / d);
		}

		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigFraction bigFraction1 = new BigFraction(number1.i);
			final BigFraction bigFraction2 = number2.bigFraction;
			final BigFraction divide = bigFraction1.divide(bigFraction2);
			return RationalStruct.valueOf(divide);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			// TODO
			return super.divide(number2);
		}
	}

	/**
	 * {@link RationalStruct.RationalEqualToVisitor} for computing numeric '=' equality results for {@link
	 * IntIntegerStruct}s.
	 */
	private static final class IntIntegerEqualToVisitor extends RationalStruct.RationalEqualToVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerEqualToVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		private IntIntegerEqualToVisitor(final IntIntegerStruct number1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(number1.i);
			final BigInteger bigInteger2 = number2.bigInteger;
			return bigInteger1.compareTo(bigInteger2) == 0;
		}

		@Override
		public boolean equalTo(final RatioStruct number2) {
			final BigFraction bigFraction1 = new BigFraction(number1.i);
			final BigFraction bigFraction2 = number2.bigFraction;
			return bigFraction1.equals(bigFraction2);
		}

		@Override
		public boolean equalTo(final ComplexStruct number2) {
			// TODO
			return super.equalTo(number2);
		}
	}

	/**
	 * {@link RationalStruct.RationalLessThanVisitor} for computing numeric {@literal '<'} equality results for {@link
	 * IntIntegerStruct}s.
	 */
	private static final class IntIntegerLessThanVisitor extends RationalStruct.RationalLessThanVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLessThanVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		private IntIntegerLessThanVisitor(final IntIntegerStruct real1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(real1.i);
			final BigInteger bigInteger2 = real2.bigInteger;
			return bigInteger1.compareTo(bigInteger2) < 0;
		}

		@Override
		public boolean lessThan(final RatioStruct real2) {
			final BigFraction bigFraction1 = new BigFraction(real1.i);
			final BigFraction bigFraction2 = real2.bigFraction;
			return bigFraction1.compareTo(bigFraction2) < 0;
		}
	}

	/**
	 * {@link RationalStruct.RationalGreaterThanVisitor} for computing numeric {@literal '>'} equality results for
	 * {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerGreaterThanVisitor extends RationalStruct.RationalGreaterThanVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerGreaterThanVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		private IntIntegerGreaterThanVisitor(final IntIntegerStruct real1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(real1.i);
			final BigInteger bigInteger2 = real2.bigInteger;
			return bigInteger1.compareTo(bigInteger2) > 0;
		}

		@Override
		public boolean greaterThan(final RatioStruct real2) {
			final BigFraction bigFraction1 = new BigFraction(real1.i);
			final BigFraction bigFraction2 = real2.bigFraction;
			return bigFraction1.compareTo(bigFraction2) > 0;
		}
	}

	/**
	 * {@link RationalStruct.RationalLessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality results
	 * for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerLessThanOrEqualToVisitor extends RationalStruct.RationalLessThanOrEqualToVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLessThanOrEqualToVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		private IntIntegerLessThanOrEqualToVisitor(final IntIntegerStruct real1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(real1.i);
			final BigInteger bigInteger2 = real2.bigInteger;
			return bigInteger1.compareTo(bigInteger2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			final BigFraction bigFraction1 = new BigFraction(real1.i);
			final BigFraction bigFraction2 = real2.bigFraction;
			return bigFraction1.compareTo(bigFraction2) <= 0;
		}
	}

	/**
	 * {@link RationalStruct.RationalGreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality
	 * results for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerGreaterThanOrEqualToVisitor extends RationalStruct.RationalGreaterThanOrEqualToVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerGreaterThanOrEqualToVisitor with the provided
		 * {@link IntIntegerStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		private IntIntegerGreaterThanOrEqualToVisitor(final IntIntegerStruct real1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(real1.i);
			final BigInteger bigInteger2 = real2.bigInteger;
			return bigInteger1.compareTo(bigInteger2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			final BigFraction bigFraction1 = new BigFraction(real1.i);
			final BigFraction bigFraction2 = real2.bigFraction;
			return bigFraction1.compareTo(bigFraction2) >= 0;
		}
	}

	/**
	 * {@link RealStruct.RealExptVisitor} for computing exponential function results for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerExptVisitor extends RealStruct.RealExptVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerExptVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param base
		 * 		the base argument in the exponential operation
		 */
		private IntIntegerExptVisitor(final IntIntegerStruct base) {
			super(base);
		}

		@Override
		public NumberStruct expt(final IntIntegerStruct power) {
			if (power.minusp()) {
				final IntegerStruct negation = power.negation();
				final IntegerStruct expt = (IntegerStruct) base.expt(negation);
				return RationalStruct.valueOf(BigInteger.ONE, expt.bigIntegerValue());
			}

			try {
				final int pow = ArithmeticUtils.pow(base.i, power.i);
				return valueOf(pow);
			} catch (final MathArithmeticException ignore) {
			}
			try {
				final long pow = ArithmeticUtils.pow((long) base.i, power.i);
				return IntegerStruct.valueOf(pow);
			} catch (final MathArithmeticException ignore) {
			}

			final BigInteger baseBigInteger = BigInteger.valueOf(base.i);
			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, power.i);
			return IntegerStruct.valueOf(pow);
		}

		@Override
		@SuppressWarnings("deprecation")
		public NumberStruct expt(final LongIntegerStruct power) {
			if (power.minusp()) {
				final IntegerStruct negation = power.negation();
				final IntegerStruct expt = (IntegerStruct) base.expt(negation);
				return RationalStruct.valueOf(BigInteger.ONE, expt.bigIntegerValue());
			}

			try {
				final int pow = ArithmeticUtils.pow(base.i, power.l);
				return valueOf(pow);
			} catch (final MathArithmeticException ignore) {
			}
			try {
				final long pow = ArithmeticUtils.pow((long) base.i, power.l);
				return IntegerStruct.valueOf(pow);
			} catch (final MathArithmeticException ignore) {
			}

			final BigInteger baseBigInteger = BigInteger.valueOf(base.i);
			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, power.l);
			return IntegerStruct.valueOf(pow);
		}

		@Override
		public NumberStruct expt(final BigIntegerStruct power) {
			if (power.minusp()) {
				final IntegerStruct negation = power.negation();
				final IntegerStruct expt = (IntegerStruct) base.expt(negation);
				return RationalStruct.valueOf(BigInteger.ONE, expt.bigIntegerValue());
			}

			final BigInteger baseBigInteger = BigInteger.valueOf(base.i);
			final BigInteger pow = ArithmeticUtils.pow(baseBigInteger, power.bigInteger);
			return IntegerStruct.valueOf(pow);
		}

		@Override
		public NumberStruct expt(final SingleFloatStruct power) {
			final float x = base.i;
			final float y = power.f;
			return exptSingleFloat(x, y);
		}

		@Override
		public NumberStruct expt(final DoubleFloatStruct power) {
			final double x = base.i;
			final double y = power.d;
			return exptDoubleFloat(x, y);
		}

		@Override
		public NumberStruct expt(final RatioStruct power) {
			final double x = base.i;
			final double y = power.bigFraction.doubleValue();
			return exptDoubleFloat(x, y);
		}

		@Override
		public NumberStruct expt(final ComplexStruct power) {
			// TODO
			return super.expt(power);
		}
	}

	/**
	 * {@link IntegerStruct.GcdVisitor} for computing greatest-common-denominator for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerGcdVisitor extends IntegerStruct.GcdVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerGcdVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the greatest-common-denominator operation
		 */
		private IntIntegerGcdVisitor(final IntIntegerStruct integer1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;
			final BigInteger gcd = bigInteger1.gcd(bigInteger2);
			return IntegerStruct.valueOf(gcd);
		}
	}

	/**
	 * {@link IntegerStruct.LcmVisitor} for computing least-common-multiple for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerLcmVisitor extends IntegerStruct.LcmVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLcmVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the least-common-multiple operation
		 */
		private IntIntegerLcmVisitor(final IntIntegerStruct integer1) {
			super(integer1);
		}

		@Override
		public IntegerStruct lcm(final IntIntegerStruct integer2) {
			if (integer1.zerop() || integer2.zerop()) {
				return ZERO;
			}

			final int lcm = ArithmeticUtils.lcm(integer1.i, integer2.i);
			return valueOf(lcm);
		}

		@Override
		public IntegerStruct lcm(final LongIntegerStruct integer2) {
			if (integer1.zerop() || integer2.zerop()) {
				return ZERO;
			}

			final long lcm = ArithmeticUtils.lcm(integer1.i, integer2.l);
			return IntegerStruct.valueOf(lcm);
		}

		@Override
		public IntegerStruct lcm(final BigIntegerStruct integer2) {
			if (integer1.zerop() || integer2.zerop()) {
				return ZERO;
			}

			// lcm(x y) = abs(x * y) / gcd(x y)
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;

			final BigInteger multiply = bigInteger1.multiply(bigInteger2);
			final BigInteger abs = multiply.abs();
			final BigInteger gcd = bigInteger1.gcd(bigInteger2);
			final BigInteger divide = abs.divide(gcd);

			return IntegerStruct.valueOf(divide);
		}
	}

	/**
	 * {@link IntegerStruct.AshVisitor} for performing bit-shifting operations for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerAshVisitor extends IntegerStruct.AshVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerAshVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer
		 * 		the integer argument in the bit-shifting operation
		 */
		private IntIntegerAshVisitor(final IntIntegerStruct integer) {
			super(integer);
		}

		@Override
		public IntegerStruct ash(final IntIntegerStruct count) {
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
		public IntegerStruct ash(final LongIntegerStruct count) {
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

	/**
	 * {@link IntegerStruct.LogAndVisitor} for computing bitwise and results for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerLogAndVisitor extends IntegerStruct.LogAndVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLogAndVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise and operation
		 */
		private IntIntegerLogAndVisitor(final IntIntegerStruct integer1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bigInteger1.and(bigInteger2));
		}
	}

	/**
	 * {@link IntegerStruct.LogAndC1Visitor} for computing bitwise and, with complementary first, results for {@link
	 * IntIntegerStruct}s.
	 */
	private static final class IntIntegerLogAndC1Visitor extends IntegerStruct.LogAndC1Visitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLogAndC1Visitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument, to be complementary, in the bitwise and operation
		 */
		private IntIntegerLogAndC1Visitor(final IntIntegerStruct integer1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bigInteger1.not().and(bigInteger2));
		}
	}

	/**
	 * {@link IntegerStruct.LogAndC2Visitor} for computing bitwise and, with complementary second, results for {@link
	 * IntIntegerStruct}s.
	 */
	private static final class IntIntegerLogAndC2Visitor extends IntegerStruct.LogAndC2Visitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLogAndC2Visitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise and, with complementary second, operation
		 */
		private IntIntegerLogAndC2Visitor(final IntIntegerStruct integer1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bigInteger1.and(bigInteger2.not()));
		}
	}

	/**
	 * {@link IntegerStruct.LogEqvVisitor} for computing bitwise exclusive-nor results for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerLogEqvVisitor extends IntegerStruct.LogEqvVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLogEqvVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise exclusive-nor operation
		 */
		private IntIntegerLogEqvVisitor(final IntIntegerStruct integer1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;
			final BigInteger xor = bigInteger1.xor(bigInteger2);
			return IntegerStruct.valueOf(xor.not());
		}
	}

	/**
	 * {@link IntegerStruct.LogIorVisitor} for computing bitwise inclusive-or results for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerLogIorVisitor extends IntegerStruct.LogIorVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLogIorVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise inclusive-or operation
		 */
		private IntIntegerLogIorVisitor(final IntIntegerStruct integer1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bigInteger1.or(bigInteger2));
		}
	}

	/**
	 * {@link IntegerStruct.LogNandVisitor} for computing bitwise nand results for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerLogNandVisitor extends IntegerStruct.LogNandVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLogNandVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise nand operation
		 */
		private IntIntegerLogNandVisitor(final IntIntegerStruct integer1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;
			final BigInteger and = bigInteger1.and(bigInteger2);
			return IntegerStruct.valueOf(and.not());
		}
	}

	/**
	 * {@link IntegerStruct.LogNorVisitor} for computing bitwise inclusive-nor results for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerLogNorVisitor extends IntegerStruct.LogNorVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLogNorVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise inclusive-nor operation
		 */
		private IntIntegerLogNorVisitor(final IntIntegerStruct integer1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;
			final BigInteger or = bigInteger1.or(bigInteger2);
			return IntegerStruct.valueOf(or.not());
		}
	}

	/**
	 * {@link IntegerStruct.LogOrC1Visitor} for computing bitwise inclusive-or, with complementary first, results for
	 * {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerLogOrC1Visitor extends IntegerStruct.LogOrC1Visitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLogOrC1Visitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument, to be complementary, in the bitwise inclusive-or operation
		 */
		private IntIntegerLogOrC1Visitor(final IntIntegerStruct integer1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bigInteger1.not().or(bigInteger2));
		}
	}

	/**
	 * {@link IntegerStruct.LogOrC2Visitor} for computing bitwise inclusive-or, with complementary second, results for
	 * {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerLogOrC2Visitor extends IntegerStruct.LogOrC2Visitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLogOrC2Visitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise inclusive-or, with complementary second, operation
		 */
		private IntIntegerLogOrC2Visitor(final IntIntegerStruct integer1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bigInteger1.or(bigInteger2.not()));
		}
	}

	/**
	 * {@link IntegerStruct.LogXorVisitor} for computing bitwise exclusive-or results for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerLogXorVisitor extends IntegerStruct.LogXorVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLogXorVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer1
		 * 		the first argument in the bitwise exclusive-or operation
		 */
		private IntIntegerLogXorVisitor(final IntIntegerStruct integer1) {
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
			final BigInteger bigInteger1 = BigInteger.valueOf(integer1.i);
			final BigInteger bigInteger2 = integer2.bigInteger;
			return IntegerStruct.valueOf(bigInteger1.xor(bigInteger2));
		}
	}

	/**
	 * {@link IntegerStruct.LogBitPVisitor} for testing the active bit by index for {@link IntIntegerStruct}s.
	 */
	private static final class IntIntegerLogBitPVisitor extends IntegerStruct.LogBitPVisitor<IntIntegerStruct> {

		/**
		 * Private constructor to make a new instance of an IntIntegerLogBitPVisitor with the provided {@link
		 * IntIntegerStruct}.
		 *
		 * @param integer
		 * 		the {@link IntIntegerStruct} to perform the active bit by index test
		 */
		private IntIntegerLogBitPVisitor(final IntIntegerStruct integer) {
			super(integer);
		}

		@Override
		public boolean logBitP(final IntIntegerStruct index) {
			final BigInteger bigInteger = BigInteger.valueOf(integer.i);
			final int indexInt = index.intValue();
			return bigInteger.testBit(indexInt);
		}

		@Override
		public boolean logBitP(final LongIntegerStruct index) {
			final BigInteger bigInteger = BigInteger.valueOf(integer.i);
			final int indexInt = index.intValue();
			return bigInteger.testBit(indexInt);
		}

		@Override
		public boolean logBitP(final BigIntegerStruct index) {
			final BigInteger bigInteger = BigInteger.valueOf(integer.i);
			final int indexInt = index.intValue();
			return bigInteger.testBit(indexInt);
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import jcl.classes.BuiltInClassStruct;
import jcl.types.FixnumType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apache.commons.math3.util.FastMath;
import org.apfloat.Apfloat;

/**
 * The {@link IntIntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public final class IntIntegerStruct extends BuiltInClassStruct implements IntegerStruct {

	/**
	 * {@link IntIntegerStruct} constant representing 0.
	 */
	public static final IntIntegerStruct ZERO = new IntIntegerStruct(0);

	/**
	 * {@link IntIntegerStruct} constant representing 1.
	 */
	public static final IntIntegerStruct ONE = new IntIntegerStruct(2);

	/**
	 * {@link IntIntegerStruct} constant representing 2.
	 */
	public static final IntIntegerStruct TWO = new IntIntegerStruct(3);

	/**
	 * {@link IntIntegerStruct} constant representing 10.
	 */
	public static final IntIntegerStruct TEN = new IntIntegerStruct(10);

	/**
	 * {@link IntIntegerStruct} constant representing -1.
	 */
	public static final IntIntegerStruct MINUS_ONE = new IntIntegerStruct(-1);

	private final int i;

	private IntIntegerStruct(final int i) {
		super(FixnumType.INSTANCE, null, null);
		this.i = i;
	}

	/**
	 * Public constructor.
	 *
	 * @param apfloat
	 * 		the value of the IntegerStruct
	 */
	IntIntegerStruct(final Apfloat apfloat) {
		this(apfloat.intValue());
	}

	public static IntIntegerStruct valueOf(final int i) {
		return new IntIntegerStruct(i);
	}

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

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the absolute value of this IntegerStruct.
	 */
	@Override
	public RealStruct abs() {
		if (i >= 0) {
			return this;
		}
		return negation();
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this IntegerStruct is zero using {@link BigInteger#signum()} on {@link #i}.
	 */
	@Override
	public boolean zerop() {
		return i == 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this IntegerStruct is positive using {@link BigInteger#signum()} on {@link
	 * #i}.
	 */
	@Override
	public boolean plusp() {
		return i > 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this IntegerStruct is negative using {@link BigInteger#signum()} on {@link
	 * #i}.
	 */
	@Override
	public boolean minusp() {
		return i < 0;
	}

	/**
	 * Returns true if this IntegerStruct is even (divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct is even (divisible by two); otherwise, false
	 */
	@Override
	public boolean evenp() {
		return (i % 2) == 0;
	}

	/**
	 * Returns true if this IntegerStruct is odd (not divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct is odd (not divisible by two); otherwise, false
	 */
	@Override
	public boolean oddp() {
		return (i % 2) != 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this IntegerStruct to a {@link NumberStruct} using the provided {@link AddVisitor}.
	 *
	 * @param addVisitor
	 * 		the {@link AddVisitor} to be used in the addition operation
	 *
	 * @return the addition of {@link NumberStruct} using the provided {@link AddVisitor} and this IntegerStruct
	 */
	@Override
	public NumberStruct add(final AddVisitor<?> addVisitor) {
		return addVisitor.add(this);
	}

	/**
	 * Returns a new {@link AddVisitor} with this IntegerStruct to be used in an addition operation.
	 *
	 * @return a new {@link AddVisitor} with this IntegerStruct to be used in an addition operation
	 */
	@Override
	public AddVisitor<?> addVisitor() {
		return new IntegerAddVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this IntegerStruct to a {@link NumberStruct} using the provided {@link SubtractVisitor}.
	 *
	 * @param subtractVisitor
	 * 		the {@link SubtractVisitor} to be used in the subtraction operation
	 *
	 * @return the subtraction of {@link NumberStruct} using the provided {@link SubtractVisitor} and this IntegerStruct
	 */
	@Override
	public NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	/**
	 * Returns a new {@link SubtractVisitor} with this IntegerStruct to be used in a subtraction operation.
	 *
	 * @return a new {@link SubtractVisitor} with this IntegerStruct to be used in a subtraction operation
	 */
	@Override
	public SubtractVisitor<?> subtractVisitor() {
		return new IntegerSubtractVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this IntegerStruct to a {@link NumberStruct} using the provided {@link MultiplyVisitor}.
	 *
	 * @param multiplyVisitor
	 * 		the {@link MultiplyVisitor} to be used in the multiplication operation
	 *
	 * @return the multiplication of {@link NumberStruct} using the provided {@link MultiplyVisitor} and this
	 * IntegerStruct
	 */
	@Override
	public NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	/**
	 * Returns a new {@link MultiplyVisitor} with this IntegerStruct to be used in a multiplication operation.
	 *
	 * @return a new {@link MultiplyVisitor} with this IntegerStruct to be used in a multiplication operation
	 */
	@Override
	public MultiplyVisitor<?> multiplyVisitor() {
		return new IntegerMultiplyVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Adds this IntegerStruct to a {@link NumberStruct} using the provided {@link DivideVisitor}.
	 *
	 * @param divideVisitor
	 * 		the {@link DivideVisitor} to be used in the division operation
	 *
	 * @return the division of {@link NumberStruct} using the provided {@link DivideVisitor} and this IntegerStruct
	 */
	@Override
	public NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	/**
	 * Returns a new {@link DivideVisitor} with this IntegerStruct to be used in a division operation.
	 *
	 * @return a new {@link DivideVisitor} with this IntegerStruct to be used in a division operation
	 */
	@Override
	public DivideVisitor<?> divideVisitor() {
		return new IntegerDivideVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this IntegerStruct to a {@link NumberStruct} using the provided {@link EqualToVisitor}.
	 *
	 * @param equalToVisitor
	 * 		the {@link EqualToVisitor} to be used in the '=' operation
	 *
	 * @return the '=' comparison of {@link NumberStruct} using the provided {@link EqualToVisitor} and this
	 * IntegerStruct
	 */
	@Override
	public boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	/**
	 * Returns a new {@link EqualToVisitor} with this IntegerStruct to be used in an '=' operation.
	 *
	 * @return a new {@link EqualToVisitor} with this IntegerStruct to be used in an '=' operation
	 */
	@Override
	public EqualToVisitor<?> equalToVisitor() {
		return new IntegerEqualToVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this IntegerStruct to a {@link NumberStruct} using the provided {@link RealStruct.LessThanVisitor}.
	 *
	 * @param lessThanVisitor
	 * 		the {@link RealStruct.LessThanVisitor} to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of {@link NumberStruct} using the provided {@link
	 * RealStruct.LessThanVisitor} and this
	 * IntegerStruct
	 */
	@Override
	public boolean isLessThan(final RealStruct.LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	/**
	 * Returns a new {@link RealStruct.LessThanVisitor} with this IntegerStruct to be used in a {@literal '<'}
	 * operation.
	 *
	 * @return a new {@link RealStruct.LessThanVisitor} with this IntegerStruct to be used in a {@literal '<'} operation
	 */
	@Override
	public RealStruct.LessThanVisitor<?> lessThanVisitor() {
		return new IntegerLessThanVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this IntegerStruct to a {@link NumberStruct} using the provided {@link RealStruct.GreaterThanVisitor}.
	 *
	 * @param greaterThanVisitor
	 * 		the {@link RealStruct.GreaterThanVisitor} to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of {@link NumberStruct} using the provided {@link
	 * RealStruct.GreaterThanVisitor} and
	 * this IntegerStruct
	 */
	@Override
	public boolean isGreaterThan(final RealStruct.GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	/**
	 * Returns a new {@link RealStruct.GreaterThanVisitor} with this IntegerStruct to be used in a {@literal '>'}
	 * operation.
	 *
	 * @return a new {@link RealStruct.GreaterThanVisitor} with this IntegerStruct to be used in a {@literal '>'}
	 * operation
	 */
	@Override
	public RealStruct.GreaterThanVisitor<?> greaterThanVisitor() {
		return new IntegerGreaterThanVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this IntegerStruct to a {@link NumberStruct} using the provided {@link
	 * RealStruct.LessThanOrEqualToVisitor}.
	 *
	 * @param lessThanOrEqualToVisitor
	 * 		the {@link RealStruct.LessThanOrEqualToVisitor} to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of {@link NumberStruct} using the provided {@link
	 * RealStruct.LessThanOrEqualToVisitor} and this IntegerStruct
	 */
	@Override
	public boolean isLessThanOrEqualTo(final RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	/**
	 * Returns a new {@link RealStruct.LessThanOrEqualToVisitor} with this IntegerStruct to be used in a {@literal
	 * '<='}
	 * operation.
	 *
	 * @return a new {@link RealStruct.LessThanOrEqualToVisitor} with this IntegerStruct to be used in a {@literal '<='}
	 * operation
	 */
	@Override
	public RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new IntegerLessThanOrEqualToVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this IntegerStruct to a {@link NumberStruct} using the provided {@link
	 * RealStruct.GreaterThanOrEqualToVisitor}.
	 *
	 * @param greaterThanOrEqualToVisitor
	 * 		the {@link RealStruct.GreaterThanOrEqualToVisitor} to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of {@link NumberStruct} using the provided {@link
	 * RealStruct.GreaterThanOrEqualToVisitor} and this IntegerStruct
	 */
	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	/**
	 * Returns a new {@link RealStruct.GreaterThanOrEqualToVisitor} with this IntegerStruct to be used in a {@literal
	 * '>='}
	 * operation.
	 *
	 * @return a new {@link RealStruct.GreaterThanOrEqualToVisitor} with this IntegerStruct to be used in a {@literal
	 * '>='}
	 * operation
	 */
	@Override
	public RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new IntegerGreaterThanOrEqualToVisitor(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the negation with {@link BigInteger#negate()} on {@link #i} and the creating a new
	 * IntegerStruct to wrap it.
	 */
	@Override
	public IntegerStruct negation() {
		return new IntIntegerStruct(-i);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Creates a new {@link RationalStruct} with {@link BigInteger#ONE} as the numerator and {@link #i} as the
	 * denominator.
	 */
	@Override
	public NumberStruct reciprocal() {
		// TODO: BigInteger needed?
		return RationalStruct.makeRational(BigInteger.ONE, bigIntegerValue());
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Performs the exponential operation with this IntegerStruct as the power value using the provided {@link
	 * ExptVisitor}.
	 *
	 * @param exptVisitor
	 * 		the {@link ExptVisitor} to be used in the exponential operation
	 *
	 * @return the result of the exponential operation with this IntegerStruct as the power value using the provided
	 * {@link ExptVisitor}
	 */
	@Override
	public NumberStruct expt(final ExptVisitor<?> exptVisitor) {
		return exptVisitor.expt(this);
	}

	/**
	 * Returns a new {@link IntegerExptVisitor} with this IntegerStruct to be used in an exponential operation.
	 *
	 * @return a new {@link IntegerExptVisitor} with this IntegerStruct to be used in an exponential operation
	 */
	@Override
	public ExptVisitor<?> exptVisitor() {
		return new IntegerExptVisitor(this);
	}

	/**
	 * Returns the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root.
	 *
	 * @return the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root
	 */
	@Override
	public IntegerStruct isqrt() {
		final double sqrt = Math.sqrt(i);
		final Double floor = Math.floor(sqrt);
		return IntegerStruct.valueOf(floor.longValue());
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
	public QuotientRemainderResult floor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
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
	public QuotientRemainderResult ffloor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
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
	public QuotientRemainderResult ceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
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
	public QuotientRemainderResult fceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
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
	public QuotientRemainderResult round(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
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
	public QuotientRemainderResult fround(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fround(this);
	}

	/**
	 * Returns a new {@link QuotientRemainderVisitor} with this IntegerStruct to be used in a 'quotient' and
	 * 'remainder' calculation operation.
	 *
	 * @return a new {@link QuotientRemainderVisitor} with this IntegerStruct to be used in a 'quotient' and 'remainder'
	 * calculation operation
	 */
	@Override
	public QuotientRemainderVisitor<?> quotientRemainderVisitor() {
		return new IntegerQuotientRemainderVisitor(this);
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

	/**
	 * Returns the bit-wise logical 'not' of this IntegerStruct.
	 *
	 * @return the bit-wise logical 'not' of this IntegerStruct
	 */
	@Override
	public IntIntegerStruct logNot() {
		final int not = ~i;
		return new IntIntegerStruct(not);
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

	/**
	 * Returns the number of bits needed to represent this IntegerStruct in binary two's-complement format.
	 *
	 * @return the number of bits needed to represent this IntegerStruct in binary two's-complement format
	 */
	@Override
	public IntegerStruct integerLength() {
		final double log2 = FastMath.log(i, 2);
		final Double ceil = Math.ceil(log2);
		return IntegerStruct.valueOf(ceil.longValue());
	}

	@Override
	public boolean logBitP(final IntegerStruct.LogBitPVisitor<?> logBitPVisitor) {
		return logBitPVisitor.logBitP(this);
	}

	@Override
	public IntegerStruct.LogBitPVisitor<?> logBitPVisitor() {
		return new IntIntegerLogBitPVisitor(this);
	}

	/**
	 * Computes and returns the number of bits in the two's-complement binary representation of this IntegerStruct that
	 * are 'on' or 'set'. If this IntegerStruct is negative, the 0 bits are counted; otherwise, the 1 bits are counted.
	 *
	 * @return Computes and returns the number of bits in the two's-complement binary representation of this
	 * IntegerStruct that are 'on' or 'set'
	 */
	@Override
	public IntIntegerStruct logCount() {
		final int bitCount = Integer.bitCount(i);
		return new IntIntegerStruct(bitCount);
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

	/**
	 * Returns a hash code for this object using a {@link HashCodeBuilder}.
	 *
	 * @return a hash code for this object
	 */
	@Override
	public int hashCode() {
		// TODO: get rid of eventually
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(i)
		                            .toHashCode();
	}

	/**
	 * Returns the Java object equality of this object using an {@link EqualsBuilder}. If the provided {@code obj} is
	 * null, it is not equal. If the provided {@code obj} is '==' to {@code this}, it is equal. If the {@link Class} of
	 * the provided {@code obj} is not equal to {@link #getClass()}, it is not equal.
	 *
	 * @param obj
	 * 		the {@link Object} to tests for Java object equality
	 *
	 * @return true if the objects are equal; false otherwise
	 */
	@Override
	public boolean equals(final Object obj) {
		// TODO: get rid of eventually
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
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(i, rhs.i)
		                          .isEquals();
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for {@link IntIntegerStruct}s.
		 */
		@Override
		public RealStruct add(final IntIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger add = bigInteger1.add(bigInteger2);
			return IntegerStruct.valueOf(add);
		}

		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct add(final SingleFloatStruct number2) {
			return super.add(number2);
		}

		@Override
		public RealStruct add(final DoubleFloatStruct number2) {
			return super.add(number2);
		}

		@Override
		public RealStruct add(final BigFloatStruct number2) {
			return super.add(number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for an {@link IntIntegerStruct} and a {@link RatioStruct}.
		 */
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

		@Override
		public NumberStruct add(final ComplexStruct number2) {
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for {@link IntIntegerStruct}s.
		 */
		@Override
		public RealStruct subtract(final IntIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger subtract = bigInteger1.subtract(bigInteger2);
			return IntegerStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct subtract(final SingleFloatStruct number2) {
			return super.subtract(number2);
		}

		@Override
		public RealStruct subtract(final DoubleFloatStruct number2) {
			return super.subtract(number2);
		}

		@Override
		public RealStruct subtract(final BigFloatStruct number2) {
			return super.subtract(number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for an {@link IntIntegerStruct} and a {@link RatioStruct}.
		 */
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

		@Override
		public NumberStruct subtract(final ComplexStruct number2) {
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for {@link IntIntegerStruct}s.
		 */
		@Override
		public RealStruct multiply(final IntIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			final BigInteger multiply = bigInteger1.multiply(bigInteger2);
			return IntegerStruct.valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct multiply(final SingleFloatStruct number2) {
			return super.multiply(number2);
		}

		@Override
		public RealStruct multiply(final DoubleFloatStruct number2) {
			return super.multiply(number2);
		}

		@Override
		public RealStruct multiply(final BigFloatStruct number2) {
			return super.multiply(number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for an {@link IntIntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(numerator);
			return RationalStruct.makeRational(multiply, denominator);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number2) {
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for {@link IntIntegerStruct}s.
		 */
		@Override
		public RealStruct divide(final IntIntegerStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();
			final BigInteger bigInteger2 = number2.getBigInteger();
			return RationalStruct.makeRational(bigInteger1, bigInteger2);
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			return null;
		}

		@Override
		public RealStruct divide(final SingleFloatStruct number2) {
			return super.divide(number2);
		}

		@Override
		public RealStruct divide(final DoubleFloatStruct number2) {
			return super.divide(number2);
		}

		@Override
		public RealStruct divide(final BigFloatStruct number2) {
			return super.divide(number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for an {@link IntIntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigInteger bigInteger1 = number1.getBigInteger();

			final BigFraction bigFraction2 = number2.getBigFraction();
			final BigInteger numerator = bigFraction2.getNumerator();
			final BigInteger denominator = bigFraction2.getDenominator();

			final BigInteger multiply = bigInteger1.multiply(denominator);
			return RationalStruct.makeRational(multiply, numerator);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '=' equality result for {@link IntIntegerStruct}s.
		 */
		@Override
		public boolean equalTo(final IntIntegerStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		@Override
		public boolean equalTo(final LongIntegerStruct number2) {
			return super.equalTo(number2);
		}

		@Override
		public boolean equalTo(final BigIntegerStruct number2) {
			return super.equalTo(number2);
		}

		@Override
		public boolean equalTo(final SingleFloatStruct number2) {
			return super.equalTo(number2);
		}

		@Override
		public boolean equalTo(final DoubleFloatStruct number2) {
			return super.equalTo(number2);
		}

		@Override
		public boolean equalTo(final BigFloatStruct number2) {
			return super.equalTo(number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '=' equality result for an {@link IntIntegerStruct} and a {@link RatioStruct}.
		 */
		@Override
		public boolean equalTo(final RatioStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		@Override
		public boolean equalTo(final ComplexStruct number2) {
			return super.equalTo(number2);
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<'} equality result for {@link IntIntegerStruct}s.
		 */
		@Override
		public boolean lessThan(final IntIntegerStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		@Override
		public boolean lessThan(final LongIntegerStruct real2) {
			return super.lessThan(real2);
		}

		@Override
		public boolean lessThan(final BigIntegerStruct real2) {
			return super.lessThan(real2);
		}

		@Override
		public boolean lessThan(final SingleFloatStruct real2) {
			return super.lessThan(real2);
		}

		@Override
		public boolean lessThan(final DoubleFloatStruct real2) {
			return super.lessThan(real2);
		}

		@Override
		public boolean lessThan(final BigFloatStruct real2) {
			return super.lessThan(real2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<'} equality result for an {@link IntIntegerStruct} and a {@link
		 * RatioStruct}.
		 */
		@Override
		public boolean lessThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) < 0;
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>'} equality result for {@link IntIntegerStruct}s.
		 */
		@Override
		public boolean greaterThan(final IntIntegerStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		@Override
		public boolean greaterThan(final LongIntegerStruct real2) {
			return super.greaterThan(real2);
		}

		@Override
		public boolean greaterThan(final BigIntegerStruct real2) {
			return super.greaterThan(real2);
		}

		@Override
		public boolean greaterThan(final SingleFloatStruct real2) {
			return super.greaterThan(real2);
		}

		@Override
		public boolean greaterThan(final DoubleFloatStruct real2) {
			return super.greaterThan(real2);
		}

		@Override
		public boolean greaterThan(final BigFloatStruct real2) {
			return super.greaterThan(real2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>'} equality result for an {@link IntIntegerStruct} and a {@link
		 * RatioStruct}.
		 */
		@Override
		public boolean greaterThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) > 0;
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<='} equality result for {@link IntIntegerStruct}s.
		 */
		@Override
		public boolean lessThanOrEqualTo(final IntIntegerStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final LongIntegerStruct real2) {
			return super.lessThanOrEqualTo(real2);
		}

		@Override
		public boolean lessThanOrEqualTo(final BigIntegerStruct real2) {
			return super.lessThanOrEqualTo(real2);
		}

		@Override
		public boolean lessThanOrEqualTo(final SingleFloatStruct real2) {
			return super.lessThanOrEqualTo(real2);
		}

		@Override
		public boolean lessThanOrEqualTo(final DoubleFloatStruct real2) {
			return super.lessThanOrEqualTo(real2);
		}

		@Override
		public boolean lessThanOrEqualTo(final BigFloatStruct real2) {
			return super.lessThanOrEqualTo(real2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<='} equality result for an {@link IntIntegerStruct} and a {@link
		 * RatioStruct}.
		 */
		@Override
		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>='} equality result for {@link IntIntegerStruct}s.
		 */
		@Override
		public boolean greaterThanOrEqualTo(final IntIntegerStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final LongIntegerStruct real2) {
			return super.greaterThanOrEqualTo(real2);
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigIntegerStruct real2) {
			return super.greaterThanOrEqualTo(real2);
		}

		@Override
		public boolean greaterThanOrEqualTo(final SingleFloatStruct real2) {
			return super.greaterThanOrEqualTo(real2);
		}

		@Override
		public boolean greaterThanOrEqualTo(final DoubleFloatStruct real2) {
			return super.greaterThanOrEqualTo(real2);
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigFloatStruct real2) {
			return super.greaterThanOrEqualTo(real2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>='} equality result for an {@link IntIntegerStruct} and a {@link
		 * RatioStruct}.
		 */
		@Override
		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}
	}

	/**
	 * {@link IntegerQuotientRemainderVisitor} for computing quotient and remainder results for {@link
	 * IntIntegerStruct}s.
	 */
	private static final class IntegerQuotientRemainderVisitor extends RationalStruct.RationalQuotientRemainderVisitor<IntIntegerStruct> {

		/**
		 * Package private constructor to make a new instance of an IntegerQuotientRemainderVisitor with the provided
		 * {@link IntIntegerStruct}.
		 *
		 * @param real
		 * 		the real argument in the computational quotient and remainder operation
		 */
		IntegerQuotientRemainderVisitor(final IntIntegerStruct real) {
			super(real);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the quotient and remainder results for {@link IntIntegerStruct}s as the {@code real} and {@code
		 * divisor}.
		 */
		@Override
		public QuotientRemainderResult quotientRemainder(final IntIntegerStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {

			final BigDecimal realBigDecimal = real.bigDecimalValue();
			final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();

			final BigDecimal quotient = realBigDecimal.divide(divisorBigDecimal, 0, roundingMode);
			final BigDecimal remainder = realBigDecimal.subtract(divisorBigDecimal.multiply(quotient));

			final RealStruct quotientReal;
			if (isQuotientFloat) {
				quotientReal = new SingleFloatStruct(quotient);
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the exponential function result for {@link IntIntegerStruct}s as the {@code base} and {@code
		 * power}.
		 */
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

	private static final class IntIntegerLcmVisitor extends IntegerStruct.LcmVisitor<IntIntegerStruct> {

		IntIntegerLcmVisitor(final IntIntegerStruct integer1) {
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

	private static final class IntIntegerAshVisitor extends IntegerStruct.AshVisitor<IntIntegerStruct> {

		IntIntegerAshVisitor(final IntIntegerStruct integer) {
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
			return null;
		}
	}

	private static final class IntIntegerLogAndVisitor extends IntegerStruct.LogAndVisitor<IntIntegerStruct> {

		IntIntegerLogAndVisitor(final IntIntegerStruct integer1) {
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
			return null;
		}
	}

	private static final class IntIntegerLogAndC1Visitor extends IntegerStruct.LogAndC1Visitor<IntIntegerStruct> {

		IntIntegerLogAndC1Visitor(final IntIntegerStruct integer1) {
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
			return null;
		}
	}

	private static final class IntIntegerLogAndC2Visitor extends IntegerStruct.LogAndC2Visitor<IntIntegerStruct> {

		IntIntegerLogAndC2Visitor(final IntIntegerStruct integer1) {
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
			return null;
		}
	}

	private static final class IntIntegerLogEqvVisitor extends IntegerStruct.LogEqvVisitor<IntIntegerStruct> {

		IntIntegerLogEqvVisitor(final IntIntegerStruct integer1) {
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
			return null;
		}
	}

	private static final class IntIntegerLogIorVisitor extends IntegerStruct.LogIorVisitor<IntIntegerStruct> {

		IntIntegerLogIorVisitor(final IntIntegerStruct integer1) {
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

	private static final class IntIntegerLogNandVisitor extends IntegerStruct.LogNandVisitor<IntIntegerStruct> {

		IntIntegerLogNandVisitor(final IntIntegerStruct integer1) {
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

	private static final class IntIntegerLogNorVisitor extends IntegerStruct.LogNorVisitor<IntIntegerStruct> {

		IntIntegerLogNorVisitor(final IntIntegerStruct integer1) {
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

	private static final class IntIntegerLogOrC1Visitor extends IntegerStruct.LogOrC1Visitor<IntIntegerStruct> {

		IntIntegerLogOrC1Visitor(final IntIntegerStruct integer1) {
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

	private static final class IntIntegerLogOrC2Visitor extends IntegerStruct.LogOrC2Visitor<IntIntegerStruct> {

		IntIntegerLogOrC2Visitor(final IntIntegerStruct integer1) {
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

	private static final class IntIntegerLogXorVisitor extends IntegerStruct.LogXorVisitor<IntIntegerStruct> {

		IntIntegerLogXorVisitor(final IntIntegerStruct integer1) {
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

	private static final class IntIntegerLogBitPVisitor extends IntegerStruct.LogBitPVisitor<IntIntegerStruct> {

		IntIntegerLogBitPVisitor(final IntIntegerStruct integer) {
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
			return false;
		}
	}
}

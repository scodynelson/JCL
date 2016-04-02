/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.List;

import jcl.util.NumberUtils;
import org.apache.commons.math3.fraction.BigFraction;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;

/**
 * The {@link RealStruct} is the object representation of a Lisp 'real' type.
 */
public interface RealStruct extends NumberStruct {

	default boolean isLessThan(final RealStruct real) {
		final LessThanVisitor<?> lessThanVisitor = lessThanVisitor();
		return real.isLessThan(lessThanVisitor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this RatioStruct to a {@link NumberStruct} using the provided {@link RealStruct.LessThanVisitor}.
	 *
	 * @param lessThanVisitor
	 * 		the {@link RealStruct.LessThanVisitor} to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of {@link NumberStruct} using the provided {@link
	 * RealStruct.LessThanVisitor} and this RatioStruct
	 */
	boolean isLessThan(LessThanVisitor<?> lessThanVisitor);

	/**
	 * Returns a new {@link RealStruct.LessThanVisitor} with this RatioStruct to be used in a {@literal '<'} operation.
	 *
	 * @return a new {@link RealStruct.LessThanVisitor} with this RatioStruct to be used in a {@literal '<'} operation
	 */
	LessThanVisitor<?> lessThanVisitor();

	static boolean isLessThan(final RealStruct real, final List<RealStruct> reals) {
		RealStruct previousReal = real;

		boolean result = true;
		for (final RealStruct currentReal : reals) {
			result = previousReal.isLessThan(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	default boolean isGreaterThan(final RealStruct real) {
		final GreaterThanVisitor<?> greaterThanVisitor = greaterThanVisitor();
		return real.isGreaterThan(greaterThanVisitor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this RatioStruct to a {@link NumberStruct} using the provided {@link RealStruct.GreaterThanVisitor}.
	 *
	 * @param greaterThanVisitor
	 * 		the {@link RealStruct.GreaterThanVisitor} to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of {@link NumberStruct} using the provided {@link
	 * RealStruct.GreaterThanVisitor} and this RatioStruct
	 */
	boolean isGreaterThan(GreaterThanVisitor<?> greaterThanVisitor);

	/**
	 * Returns a new {@link RealStruct.GreaterThanVisitor} with this RatioStruct to be used in a {@literal '>'}
	 * operation.
	 *
	 * @return a new {@link RealStruct.GreaterThanVisitor} with this RatioStruct to be used in a {@literal '>'}
	 * operation
	 */
	GreaterThanVisitor<?> greaterThanVisitor();

	static boolean isGreaterThan(final RealStruct real, final List<RealStruct> reals) {
		RealStruct previousReal = real;

		boolean result = true;
		for (final RealStruct currentReal : reals) {
			result = previousReal.isGreaterThan(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	default boolean isLessThanOrEqualTo(final RealStruct real) {
		final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor = lessThanOrEqualToVisitor();
		return real.isLessThanOrEqualTo(lessThanOrEqualToVisitor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this RatioStruct to a {@link NumberStruct} using the provided {@link
	 * RealStruct.LessThanOrEqualToVisitor}.
	 *
	 * @param lessThanOrEqualToVisitor
	 * 		the {@link RealStruct.LessThanOrEqualToVisitor} to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of {@link NumberStruct} using the provided {@link
	 * RealStruct.LessThanOrEqualToVisitor} and this RatioStruct
	 */
	boolean isLessThanOrEqualTo(LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor);

	/**
	 * Returns a new {@link RealStruct.LessThanOrEqualToVisitor} with this RatioStruct to be used in a {@literal '<='}
	 * operation.
	 *
	 * @return a new {@link RealStruct.LessThanOrEqualToVisitor} with this RatioStruct to be used in a {@literal '<='}
	 * operation
	 */
	LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor();

	static boolean isLessThanOrEqualTo(final RealStruct real, final List<RealStruct> reals) {
		RealStruct previousReal = real;

		boolean result = true;
		for (final RealStruct currentReal : reals) {
			result = previousReal.isLessThanOrEqualTo(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	default boolean isGreaterThanOrEqualTo(final RealStruct real) {
		final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor = greaterThanOrEqualToVisitor();
		return real.isGreaterThanOrEqualTo(greaterThanOrEqualToVisitor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Compares this RatioStruct to a {@link NumberStruct} using the provided {@link
	 * RealStruct.GreaterThanOrEqualToVisitor}.
	 *
	 * @param greaterThanOrEqualToVisitor
	 * 		the {@link RealStruct.GreaterThanOrEqualToVisitor} to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of {@link NumberStruct} using the provided {@link
	 * RealStruct.GreaterThanOrEqualToVisitor} and this RatioStruct
	 */
	boolean isGreaterThanOrEqualTo(GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor);

	/**
	 * Returns a new {@link RealStruct.GreaterThanOrEqualToVisitor} with this RatioStruct to be used in a {@literal
	 * '>='} operation.
	 *
	 * @return a new {@link RealStruct.GreaterThanOrEqualToVisitor} with this RatioStruct to be used in a {@literal
	 * '>='} operation
	 */
	GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor();

	static boolean isGreaterThanOrEqualTo(final RealStruct real, final List<RealStruct> reals) {
		RealStruct previousReal = real;

		boolean result = true;
		for (final RealStruct currentReal : reals) {
			result = previousReal.isGreaterThanOrEqualTo(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this RatioStruct is positive by comparing {@code #bigFraction} to {@link
	 * BigFraction#ZERO}.
	 */
	boolean plusp();

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this RatioStruct is negative by comparing {@code #bigFraction} to {@link
	 * BigFraction#ZERO}.
	 */
	boolean minusp();

	default RealStruct max(final RealStruct real) {
		return isGreaterThanOrEqualTo(real) ? this : real;
	}

	static RealStruct max(final RealStruct real, final List<RealStruct> reals) {
		if (reals.isEmpty()) {
			return real;
		}
		return reals.stream().reduce(real, RealStruct::max);
	}

	default RealStruct min(final RealStruct real) {
		return isLessThanOrEqualTo(real) ? this : real;
	}

	static RealStruct min(final RealStruct real, final List<RealStruct> reals) {
		if (reals.isEmpty()) {
			return real;
		}
		return reals.stream().reduce(real, RealStruct::min);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@code this} as any RationalStruct is already in rational form.
	 */
	RationalStruct rational();

	FloatStruct floatingPoint();

	default FloatStruct floatingPoint(final FloatStruct prototype) {
		final FloatingPointVisitor<?> floatingPointVisitor = prototype.floatingPointVisitor();
		return floatingPoint(floatingPointVisitor);
	}

	FloatStruct floatingPoint(FloatingPointVisitor<?> floatingPointVisitor);

	default RealStruct mod(final RealStruct divisor) {
		final QuotientRemainderResult floor = floor(divisor);
		return floor.getRemainder();
	}

	default RealStruct rem(final RealStruct divisor) {
		final QuotientRemainderResult truncate = truncate(divisor);
		return truncate.getRemainder();
	}

	default ComplexStruct cis() {
		return new ComplexStruct(cos(), sin());
	}

	default QuotientRemainderResult floor() {
		return floor(IntegerStruct.ONE);
	}

	default QuotientRemainderResult floor(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.floor(quotientRemainderVisitor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'FLOOR' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'FLOOR' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'FLOOR' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	QuotientRemainderResult floor(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	default QuotientRemainderResult ffloor() {
		return ffloor(IntegerStruct.ONE);
	}

	default QuotientRemainderResult ffloor(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.ffloor(quotientRemainderVisitor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'FLOOR' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}. The resulting 'quotient' will be a {@link FloatStruct}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'FLOOR' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'FLOOR' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	QuotientRemainderResult ffloor(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	default QuotientRemainderResult ceiling() {
		return ceiling(IntegerStruct.ONE);
	}

	default QuotientRemainderResult ceiling(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.ceiling(quotientRemainderVisitor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'CEILING' operation with this RatioStruct as the
	 * 'divisor' using the provided {@link QuotientRemainderVisitor}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'CEILING' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'CEILING' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	QuotientRemainderResult ceiling(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	default QuotientRemainderResult fceiling() {
		return fceiling(IntegerStruct.ONE);
	}

	default QuotientRemainderResult fceiling(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.fceiling(quotientRemainderVisitor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'CEILING' operation with this RatioStruct as the
	 * 'divisor' using the provided {@link QuotientRemainderVisitor}. The resulting 'quotient' will be a {@link
	 * FloatStruct}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'CEILING' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'CEILING' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	QuotientRemainderResult fceiling(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	default QuotientRemainderResult truncate() {
		return truncate(IntegerStruct.ONE);
	}

	default QuotientRemainderResult truncate(final RealStruct divisor) {
		if ((plusp() && divisor.plusp()) || (minusp() && divisor.minusp())) {
			return floor(divisor);
		} else {
			return ceiling(divisor);
		}
	}

	default QuotientRemainderResult ftruncate() {
		return ftruncate(IntegerStruct.ONE);
	}

	default QuotientRemainderResult ftruncate(final RealStruct divisor) {
		if (plusp()) {
			return ffloor(divisor);
		} else {
			return fceiling(divisor);
		}
	}

	default QuotientRemainderResult round() {
		return round(IntegerStruct.ONE);
	}

	default QuotientRemainderResult round(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.round(quotientRemainderVisitor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'ROUND' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'ROUND' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'ROUND' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	QuotientRemainderResult round(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	default QuotientRemainderResult fround() {
		return fround(IntegerStruct.ONE);
	}

	default QuotientRemainderResult fround(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.fround(quotientRemainderVisitor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Calculates the {@link QuotientRemainderResult} for a 'ROUND' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}. The resulting 'quotient' will be a {@link FloatStruct}.
	 *
	 * @param quotientRemainderVisitor
	 * 		the {@link QuotientRemainderVisitor} to be used in the 'ROUND' operation
	 *
	 * @return the {@link QuotientRemainderResult} for a 'ROUND' operation with this RatioStruct as the 'divisor'
	 * using the provided {@link QuotientRemainderVisitor}
	 */
	QuotientRemainderResult fround(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	/**
	 * Returns a new {@link QuotientRemainderVisitor} with this RatioStruct to be used in a 'quotient' and 'remainder'
	 * calculation operation.
	 *
	 * @return a new {@link QuotientRemainderVisitor} with this RatioStruct to be used in a 'quotient' and 'remainder'
	 * calculation operation
	 */
	QuotientRemainderVisitor<?> quotientRemainderVisitor();

	/*
		NumberStruct
	 */

	@Override
	default RealStruct realPart() {
		return this;
	}

	@Override
	default RealStruct conjugate() {
		return this;
	}

	@Override
	default RealStruct exp() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat exp = ApfloatMath.exp(apfloat);
		return FloatStruct.valueOf(exp);
	}

	@Override
	default NumberStruct log() {
		final Apfloat apfloat = apfloatValue();
		if (apfloat.signum() < 0) {
			final Apcomplex log = ApcomplexMath.log(apfloat);
			return ComplexStruct.makeComplexOrReal(log);
		}
		final Apfloat log = ApfloatMath.log(apfloat);
		return FloatStruct.valueOf(log);
	}

	@Override
	default NumberStruct log(final NumberStruct base) {
		final Apfloat apfloat = apfloatValue();
		if ((apfloat.signum() < 0) || (base instanceof ComplexStruct)) {
			final Apcomplex baseVal = base.apcomplexValue();
			final Apcomplex log = ApcomplexMath.log(apfloat, baseVal);
			return ComplexStruct.makeComplexOrReal(log);
		}
		final Apfloat baseVal = ((RealStruct) base).apfloatValue();
		final Apfloat log = ApfloatMath.log(apfloat, baseVal);
		return FloatStruct.valueOf(log);
	}

	@Override
	default NumberStruct sqrt() {
		final Apfloat apfloat = apfloatValue();
		if (apfloat.signum() < 0) {
			final Apcomplex sqrt = ApcomplexMath.sqrt(apfloat);
			return ComplexStruct.makeComplexOrReal(sqrt);
		}
		final Apfloat sqrt = ApfloatMath.sqrt(apfloat);
		return FloatStruct.valueOf(sqrt);
	}

	@Override
	default RealStruct sin() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat sin = ApfloatMath.sin(apfloat);
		return FloatStruct.valueOf(sin);
	}

	@Override
	default RealStruct cos() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat cos = ApfloatMath.cos(apfloat);
		return FloatStruct.valueOf(cos);
	}

	@Override
	default RealStruct tan() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat tan = ApfloatMath.tan(apfloat);
		return FloatStruct.valueOf(tan);
	}

	@Override
	default RealStruct asin() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat asin = ApfloatMath.asin(apfloat);
		return FloatStruct.valueOf(asin);
	}

	@Override
	default RealStruct acos() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat acos = ApfloatMath.acos(apfloat);
		return FloatStruct.valueOf(acos);
	}

	@Override
	default RealStruct atan() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat atan = ApfloatMath.atan(apfloat);
		return FloatStruct.valueOf(atan);
	}

	default RealStruct atan(final RealStruct real) {
		final Apfloat apfloat1 = apfloatValue();
		final Apfloat apfloat2 = real.apfloatValue();

		final Apfloat atan = ApfloatMath.atan2(apfloat1, apfloat2);
		return FloatStruct.valueOf(atan);
	}

	@Override
	default RealStruct sinh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat sinh = ApfloatMath.sinh(apfloat);
		return FloatStruct.valueOf(sinh);
	}

	@Override
	default RealStruct cosh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat cosh = ApfloatMath.cosh(apfloat);
		return FloatStruct.valueOf(cosh);
	}

	@Override
	default RealStruct tanh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat tanh = ApfloatMath.tanh(apfloat);
		return FloatStruct.valueOf(tanh);
	}

	@Override
	default RealStruct asinh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat asinh = ApfloatMath.asinh(apfloat);
		return FloatStruct.valueOf(asinh);
	}

	@Override
	default RealStruct acosh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat acosh = ApfloatMath.acosh(apfloat);
		return FloatStruct.valueOf(acosh);
	}

	@Override
	default RealStruct atanh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat atanh = ApfloatMath.atanh(apfloat);
		return FloatStruct.valueOf(atanh);
	}

	// Visitor Implementations

	abstract class RealAddVisitor<S extends RealStruct> extends NumberStruct.AddVisitor<S> {

		RealAddVisitor(final S number1) {
			super(number1);
		}

		@Override
		public NumberStruct add(final ComplexStruct number2) {
			final Apcomplex apfloat1 = number1.apfloatValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex add = apfloat1.add(apcomplex2);
			return ComplexStruct.makeComplexOrReal(add);
		}
	}

	abstract class RealSubtractVisitor<S extends RealStruct> extends NumberStruct.SubtractVisitor<S> {

		RealSubtractVisitor(final S number1) {
			super(number1);
		}

		@Override
		public NumberStruct subtract(final ComplexStruct number2) {
			final Apcomplex apfloat1 = number1.apfloatValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex subtract = apfloat1.subtract(apcomplex2);
			return ComplexStruct.makeComplexOrReal(subtract);
		}
	}

	abstract class RealMultiplyVisitor<S extends RealStruct> extends NumberStruct.MultiplyVisitor<S> {

		RealMultiplyVisitor(final S number1) {
			super(number1);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number2) {
			final Apcomplex apfloat1 = number1.apfloatValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex multiply = apfloat1.multiply(apcomplex2);
			return ComplexStruct.makeComplexOrReal(multiply);
		}
	}

	abstract class RealDivideVisitor<S extends RealStruct> extends NumberStruct.DivideVisitor<S> {

		RealDivideVisitor(final S number1) {
			super(number1);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			final Apcomplex apfloat1 = number1.apfloatValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex divide = apfloat1.divide(apcomplex2);
			return ComplexStruct.makeComplexOrReal(divide);
		}
	}

	abstract class RealEqualToVisitor<S extends RealStruct> extends NumberStruct.EqualToVisitor<S> {

		RealEqualToVisitor(final S number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final ComplexStruct number2) {
			return number2.isEqualTo(number1);
		}
	}

	abstract class LessThanVisitor<S extends RealStruct> {

		final S real1;

		LessThanVisitor(final S real1) {
			this.real1 = real1;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<'} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean lessThan(final IntIntegerStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<'} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean lessThan(final LongIntegerStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<'} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean lessThan(final BigIntegerStruct real2);

		public abstract boolean lessThan(final SingleFloatStruct real2);

		public abstract boolean lessThan(final DoubleFloatStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<'} equality result for {@link RatioStruct}s.
		 */
		public abstract boolean lessThan(final RatioStruct real2);
	}

	abstract class GreaterThanVisitor<S extends RealStruct> {

		final S real1;

		GreaterThanVisitor(final S real1) {
			this.real1 = real1;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>'} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean greaterThan(final IntIntegerStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>'} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean greaterThan(final LongIntegerStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>'} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean greaterThan(final BigIntegerStruct real2);

		public abstract boolean greaterThan(final SingleFloatStruct real2);

		public abstract boolean greaterThan(final DoubleFloatStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>'} equality result for {@link RatioStruct}s.
		 */
		public abstract boolean greaterThan(final RatioStruct real2);
	}

	abstract class LessThanOrEqualToVisitor<S extends RealStruct> {

		final S real1;

		LessThanOrEqualToVisitor(final S real1) {
			this.real1 = real1;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<='} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean lessThanOrEqualTo(final IntIntegerStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<='} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean lessThanOrEqualTo(final LongIntegerStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<='} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean lessThanOrEqualTo(final BigIntegerStruct real2);

		public abstract boolean lessThanOrEqualTo(final SingleFloatStruct real2);

		public abstract boolean lessThanOrEqualTo(final DoubleFloatStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '<='} equality result for {@link RatioStruct}s.
		 */
		public abstract boolean lessThanOrEqualTo(final RatioStruct real2);
	}

	abstract class GreaterThanOrEqualToVisitor<S extends RealStruct> {

		final S real1;

		GreaterThanOrEqualToVisitor(final S real1) {
			this.real1 = real1;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>='} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean greaterThanOrEqualTo(final IntIntegerStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>='} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean greaterThanOrEqualTo(final LongIntegerStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>='} equality result for an {@link RatioStruct} and a {@link IntegerStruct}.
		 */
		public abstract boolean greaterThanOrEqualTo(final BigIntegerStruct real2);

		public abstract boolean greaterThanOrEqualTo(final SingleFloatStruct real2);

		public abstract boolean greaterThanOrEqualTo(final DoubleFloatStruct real2);

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric {@literal '>='} equality result for {@link RatioStruct}s.
		 */
		public abstract boolean greaterThanOrEqualTo(final RatioStruct real2);
	}

	abstract class FloatingPointVisitor<S extends FloatStruct> {

		final S prototype;

		FloatingPointVisitor(final S prototype) {
			this.prototype = prototype;
		}

		public abstract FloatStruct floatingPoint(IntIntegerStruct real);

		public abstract FloatStruct floatingPoint(LongIntegerStruct real);

		public abstract FloatStruct floatingPoint(BigIntegerStruct real);

		public abstract FloatStruct floatingPoint(SingleFloatStruct real);

		public abstract FloatStruct floatingPoint(DoubleFloatStruct real);

		public abstract FloatStruct floatingPoint(RatioStruct real);
	}

	abstract class QuotientRemainderVisitor<S extends RealStruct> {

		final S real;

		QuotientRemainderVisitor(final S real) {
			this.real = real;
		}

		public QuotientRemainderResult floor(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, false);
		}

		public QuotientRemainderResult floor(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, false);
		}

		public QuotientRemainderResult floor(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, false);
		}

		public QuotientRemainderResult ffloor(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, true);
		}

		public QuotientRemainderResult ffloor(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, true);
		}

		public QuotientRemainderResult ffloor(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, true);
		}

		public QuotientRemainderResult ceiling(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, false);
		}

		public QuotientRemainderResult ceiling(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, false);
		}

		public QuotientRemainderResult ceiling(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, false);
		}

		public QuotientRemainderResult fceiling(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, true);
		}

		public QuotientRemainderResult fceiling(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, true);
		}

		public QuotientRemainderResult fceiling(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, true);
		}

		public QuotientRemainderResult round(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, false);
		}

		public QuotientRemainderResult round(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, false);
		}

		public QuotientRemainderResult round(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, false);
		}

		public QuotientRemainderResult fround(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, true);
		}

		public QuotientRemainderResult fround(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, true);
		}

		public QuotientRemainderResult fround(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, true);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the quotient and remainder results for a {@link RatioStruct} as the {@code real} and an {@link
		 * IntegerStruct} as the {@code divisor}.
		 */
		public QuotientRemainderResult quotientRemainder(final IntegerStruct divisor, final RoundingMode roundingMode, final boolean isQuotientFloat) {
			final BigDecimal realBigDecimal = real.bigDecimalValue();
			final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();

			final BigDecimal quotient = realBigDecimal.divide(divisorBigDecimal, 0, roundingMode);
			final BigDecimal remainder = realBigDecimal.subtract(divisorBigDecimal.multiply(quotient));

			final RealStruct quotientReal;
			if (isQuotientFloat) {
				quotientReal = DoubleFloatStruct.valueOf(quotient.doubleValue());
			} else {
				final BigInteger quotientBigInteger = quotient.toBigInteger();
				quotientReal = IntegerStruct.valueOf(quotientBigInteger);
			}

			final BigInteger remainderBigInteger = remainder.toBigInteger();
			final IntegerStruct remainderInteger = IntegerStruct.valueOf(remainderBigInteger);

			return new QuotientRemainderResult(quotientReal, remainderInteger);
		}

		public QuotientRemainderResult quotientRemainder(final FloatStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			return floatQuotientRemainder(divisor, roundingMode, isQuotientFloat);
		}

		QuotientRemainderResult floatQuotientRemainder(final RealStruct divisor, final RoundingMode roundingMode,
		                                               final boolean isQuotientFloat) {
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

			final FloatStruct remainderFloat = DoubleFloatStruct.valueOf(remainder.doubleValue());
			return new QuotientRemainderResult(quotientReal, remainderFloat);
		}

		RealStruct getFloatQuotient(final RealStruct divisor, final BigDecimal quotient) {
			final RealStruct floatQuotient;
			if (BigDecimal.ZERO.compareTo(quotient) == 0) {
				if (real.minusp()) {
					if (divisor.minusp()) {
						floatQuotient = SingleFloatStruct.ZERO;
					} else {
						floatQuotient = SingleFloatStruct.MINUS_ZERO;
					}
				} else if (divisor.minusp()) {
					floatQuotient = SingleFloatStruct.MINUS_ZERO;
				} else {
					floatQuotient = SingleFloatStruct.ZERO;
				}
			} else {
				floatQuotient = DoubleFloatStruct.valueOf(quotient.doubleValue());
			}
			return floatQuotient;
		}


		public abstract QuotientRemainderResult quotientRemainder(RatioStruct divisor, RoundingMode roundingMode,
		                                                          boolean isQuotientFloat);
	}

	abstract class RealExptVisitor<S extends RealStruct> extends NumberStruct.ExptVisitor<S> {

		RealExptVisitor(final S base) {
			super(base);
		}

		@Override
		public abstract NumberStruct expt(final IntIntegerStruct power);

		@Override
		public abstract NumberStruct expt(final LongIntegerStruct power);

		@Override
		public abstract NumberStruct expt(final BigIntegerStruct power);

		@Override
		public abstract NumberStruct expt(final SingleFloatStruct power);

		@Override
		public abstract NumberStruct expt(final DoubleFloatStruct power);

		@Override
		public abstract NumberStruct expt(final RatioStruct power);

		static NumberStruct exptSingleFloat(final float x, final float y) {

			float result = NumberUtils.pow(x, y);
			if (Float.isNaN(result)) {
				if (x < 0) {
					result = NumberUtils.pow(-x, y);
					final double realPart = result * StrictMath.cos(y * Math.PI);
					final double imagPart = result * StrictMath.sin(y * Math.PI);

					final SingleFloatStruct real = SingleFloatStruct.valueOf(realPart);
					final SingleFloatStruct imaginary = SingleFloatStruct.valueOf(imagPart);
					return ComplexStruct.makeComplexOrReal(real, imaginary);
				}
			}
			return SingleFloatStruct.valueOf(result);
		}

		static NumberStruct exptDoubleFloat(final double x, final double y) {

			double result = StrictMath.pow(x, y);
			if (Double.isNaN(result)) {
				if (x < 0) {
					result = StrictMath.pow(-x, y);
					final double realPart = result * StrictMath.cos(y * Math.PI);
					final double imagPart = result * StrictMath.sin(y * Math.PI);

					final FloatStruct real = DoubleFloatStruct.valueOf(realPart);
					final FloatStruct imaginary = DoubleFloatStruct.valueOf(imagPart);
					return ComplexStruct.makeComplexOrReal(real, imaginary);
				}
			}
			return DoubleFloatStruct.valueOf(result);
		}

		@Override
		public NumberStruct expt(final ComplexStruct power) {
			final RealStruct powerComplexReal = power.getReal();
			final FloatStruct real = powerComplexReal.floatingPoint();

			final RealStruct powerComplexImaginary = power.getImaginary();
			final FloatStruct imaginary = powerComplexImaginary.floatingPoint();

			final NumberStruct newPowerComplex = ComplexStruct.makeComplexOrReal(real, imaginary);

			final RealStruct newBase = base.floatingPoint();
			final NumberStruct logOfNewBase = newBase.log();
			final NumberStruct powerComplexLogOfNewBaseProduct = newPowerComplex.multiply(logOfNewBase);
			return powerComplexLogOfNewBaseProduct.exp();
		}
	}

	/*
		Deprecated
	 */

	@Deprecated
	BigDecimal bigDecimalValue();

	@Deprecated
	default Apfloat apfloatValue() {
		return new Apfloat(bigDecimalValue());
	}

	@Override
	@Deprecated
	default Apcomplex apcomplexValue() {
		return apfloatValue();
	}
}

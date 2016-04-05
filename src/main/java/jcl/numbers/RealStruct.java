/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

import jcl.util.NumberUtils;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;

/**
 * The {@link RealStruct} is the object representation of a Lisp 'real' type.
 */
public interface RealStruct extends NumberStruct {

	/**
	 * Performs a {@literal '<'} comparison of this RealStruct and the provided RealStruct.
	 *
	 * @param real
	 * 		the RealStruct to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of this RealStruct and the provided RealStruct
	 */
	default boolean isLessThan(final RealStruct real) {
		final LessThanVisitor<?> lessThanVisitor = lessThanVisitor();
		return real.isLessThan(lessThanVisitor);
	}

	/**
	 * Performs a {@literal '<'} comparison of this RealStruct using the provided {@link LessThanVisitor}.
	 *
	 * @param lessThanVisitor
	 * 		the {@link LessThanVisitor} to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of this RealStruct using the provided {@link LessThanVisitor}
	 */
	boolean isLessThan(LessThanVisitor<?> lessThanVisitor);

	/**
	 * Returns a new {@link LessThanVisitor} with this RealStruct to be used in a {@literal '<'} comparison.
	 *
	 * @return a new {@link LessThanVisitor} with this RealStruct to be used in a {@literal '<'} comparison
	 */
	LessThanVisitor<?> lessThanVisitor();

	/**
	 * Performs a {@literal '<'} comparison of the provided RealStructs in order, using the single {@code real} as the
	 * starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct used in the {@literal '<'} comparison
	 * @param reals
	 * 		the RealStructs used in the {@literal '<'} comparison
	 *
	 * @return the {@literal '<'} comparison provided RealStruct
	 */
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

	/**
	 * Performs a {@literal '>'} comparison of this RealStruct and the provided RealStruct.
	 *
	 * @param real
	 * 		the RealStruct to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of this RealStruct and the provided RealStruct
	 */
	default boolean isGreaterThan(final RealStruct real) {
		final GreaterThanVisitor<?> greaterThanVisitor = greaterThanVisitor();
		return real.isGreaterThan(greaterThanVisitor);
	}

	/**
	 * Performs a {@literal '>'} comparison of this RealStruct using the provided {@link GreaterThanVisitor}.
	 *
	 * @param greaterThanVisitor
	 * 		the {@link GreaterThanVisitor} to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of this RealStruct using the provided {@link GreaterThanVisitor}
	 */
	boolean isGreaterThan(GreaterThanVisitor<?> greaterThanVisitor);

	/**
	 * Returns a new {@link GreaterThanVisitor} with this RealStruct to be used in a {@literal '>'} comparison.
	 *
	 * @return a new {@link GreaterThanVisitor} with this RealStruct to be used in a {@literal '>'} comparison
	 */
	GreaterThanVisitor<?> greaterThanVisitor();

	/**
	 * Performs a {@literal '>'} comparison of the provided RealStructs in order, using the single {@code real} as the
	 * starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct used in the {@literal '>'} comparison
	 * @param reals
	 * 		the RealStructs used in the {@literal '>'} comparison
	 *
	 * @return the {@literal '>'} comparison provided RealStruct
	 */
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

	/**
	 * Performs a {@literal '<='} comparison of this RealStruct and the provided RealStruct.
	 *
	 * @param real
	 * 		the RealStruct to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of this RealStruct and the provided RealStruct
	 */
	default boolean isLessThanOrEqualTo(final RealStruct real) {
		final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor = lessThanOrEqualToVisitor();
		return real.isLessThanOrEqualTo(lessThanOrEqualToVisitor);
	}

	/**
	 * Performs a {@literal '<='} comparison of this RealStruct using the provided {@link LessThanOrEqualToVisitor}.
	 *
	 * @param lessThanOrEqualToVisitor
	 * 		the {@link LessThanOrEqualToVisitor} to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of this RealStruct using the provided {@link LessThanOrEqualToVisitor}
	 */
	boolean isLessThanOrEqualTo(LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor);

	/**
	 * Returns a new {@link LessThanOrEqualToVisitor} with this RealStruct to be used in a {@literal '<='} comparison.
	 *
	 * @return a new {@link LessThanOrEqualToVisitor} with this RealStruct to be used in a {@literal '<='} comparison
	 */
	LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor();

	/**
	 * Performs a {@literal '<='} comparison of the provided RealStructs in order, using the single {@code real} as the
	 * starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct used in the {@literal '<='} comparison
	 * @param reals
	 * 		the RealStructs used in the {@literal '<='} comparison
	 *
	 * @return the {@literal '<='} comparison provided RealStruct
	 */
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

	/**
	 * Performs a {@literal '>='} comparison of this RealStruct and the provided RealStruct.
	 *
	 * @param real
	 * 		the RealStruct to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of this RealStruct and the provided RealStruct
	 */
	default boolean isGreaterThanOrEqualTo(final RealStruct real) {
		final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor = greaterThanOrEqualToVisitor();
		return real.isGreaterThanOrEqualTo(greaterThanOrEqualToVisitor);
	}

	/**
	 * Performs a {@literal '>='} comparison of this RealStruct using the provided {@link GreaterThanOrEqualToVisitor}.
	 *
	 * @param greaterThanOrEqualToVisitor
	 * 		the {@link GreaterThanOrEqualToVisitor} to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of this RealStruct using the provided {@link GreaterThanOrEqualToVisitor}
	 */
	boolean isGreaterThanOrEqualTo(GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor);

	/**
	 * Returns a new {@link GreaterThanOrEqualToVisitor} with this RealStruct to be used in a {@literal '>='}
	 * comparison.
	 *
	 * @return a new {@link GreaterThanOrEqualToVisitor} with this RealStruct to be used in a {@literal '>='} comparison
	 */
	GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor();

	/**
	 * Performs a {@literal '>='} comparison of the provided RealStructs in order, using the single {@code real} as the
	 * starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct used in the {@literal '>='} comparison
	 * @param reals
	 * 		the RealStructs used in the {@literal '>='} comparison
	 *
	 * @return the {@literal '>='} comparison provided RealStruct
	 */
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
	 * Determines whether or not this RealStruct is positive.
	 *
	 * @return true if this RealStruct is positive; false otherwise
	 */
	boolean plusp();

	/**
	 * Determines whether or not this RealStruct is negative.
	 *
	 * @return true if this RealStruct is negative; false otherwise
	 */
	boolean minusp();

	/**
	 * Returns the most positive value when comparing the value of this RealStruct and the provided RealStruct.
	 *
	 * @param real
	 * 		the RealStruct used and possibly returned if it is greater than this RealStruct
	 *
	 * @return the most positive value when comparing the value of this RealStruct and the provided RealStruct
	 */
	default RealStruct max(final RealStruct real) {
		return isGreaterThanOrEqualTo(real) ? this : real;
	}

	/**
	 * Returns the most positive value when comparing the values of the provided RealStructs in order, using the single
	 * {@code real} as the starting point in the comparison.
	 *
	 * @param real
	 * 		the initial RealStruct used in the comparison, and possibly returned if the {@link List} of RealStructs is
	 * 		empty
	 * @param reals
	 * 		the RealStructs used in the comparison in determining the greatest value
	 *
	 * @return the most positive value when comparing the value of the initial RealStruct and the provided {@link List}
	 * of RealStructs
	 */
	static RealStruct max(final RealStruct real, final List<RealStruct> reals) {
		if (reals.isEmpty()) {
			return real;
		}
		return reals.stream().reduce(real, RealStruct::max);
	}

	/**
	 * Returns the least positive value when comparing the value of this RealStruct and the provided RealStruct.
	 *
	 * @param real
	 * 		the RealStruct used and possibly returned if it is less than this RealStruct
	 *
	 * @return the least positive value when comparing the value of this RealStruct and the provided RealStruct
	 */
	default RealStruct min(final RealStruct real) {
		return isLessThanOrEqualTo(real) ? this : real;
	}

	/**
	 * Returns the least positive value when comparing the values of the provided RealStructs in order, using the
	 * single {@code real} as the starting point in the comparison.
	 *
	 * @param real
	 * 		the initial RealStruct used in the comparison, and possibly returned if the {@link List} of RealStructs is
	 * 		empty
	 * @param reals
	 * 		the RealStructs used in the comparison in determining the least greatest value
	 *
	 * @return the least positive value when comparing the value of the initial RealStruct and the provided {@link List}
	 * of RealStructs
	 */
	static RealStruct min(final RealStruct real, final List<RealStruct> reals) {
		if (reals.isEmpty()) {
			return real;
		}
		return reals.stream().reduce(real, RealStruct::min);
	}

	/**
	 * Returns this RealStruct as a representational {@link RationalStruct}.
	 *
	 * @return this RealStruct as a representational {@link RationalStruct}
	 */
	RationalStruct rational();

	/**
	 * Returns this RealStruct as a representational {@link FloatStruct}.
	 *
	 * @return this RealStruct as a representational {@link FloatStruct}
	 */
	FloatStruct floatingPoint();

	/**
	 * Returns this RealStruct as a representational {@link FloatStruct}, using the prototype as the type of {@link
	 * FloatStruct} to return.
	 *
	 * @param prototype
	 * 		an object representing the type of {@link FloatStruct} to return
	 *
	 * @return this RealStruct as a representational {@link FloatStruct}
	 */
	default FloatStruct floatingPoint(final FloatStruct prototype) {
		final FloatingPointVisitor<?> floatingPointVisitor = prototype.floatingPointVisitor();
		return floatingPoint(floatingPointVisitor);
	}

	/**
	 * Returns this RealStruct as a representational {@link FloatStruct} using the provided {@link
	 * FloatingPointVisitor}.
	 *
	 * @param floatingPointVisitor
	 * 		the {@link FloatingPointVisitor} to be used in the {@link FloatStruct} conversion
	 *
	 * @return this RealStruct as a representational {@link FloatStruct} using the provided {@link FloatingPointVisitor}
	 */
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
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.truncate(quotientRemainderVisitor);
	}

	QuotientRemainderResult truncate(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	default QuotientRemainderResult ftruncate() {
		return ftruncate(IntegerStruct.ONE);
	}

	default QuotientRemainderResult ftruncate(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.ftruncate(quotientRemainderVisitor);
	}

	QuotientRemainderResult ftruncate(QuotientRemainderVisitor<?> quotientRemainderVisitor);

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

	/**
	 * {@link LessThanVisitor} for computing numeric {@literal '<'} equality for {@link RealStruct}s.
	 */
	abstract class LessThanVisitor<S extends RealStruct> {

		/**
		 * The {@link S} as the first argument in the equality operation.
		 */
		final S real1;

		/**
		 * Package private constructor.
		 *
		 * @param real1
		 * 		the first argument in the equality operation
		 */
		LessThanVisitor(final S real1) {
			this.real1 = real1;
		}

		/**
		 * Computes the numeric {@literal '<'} equality for an {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than the provided {@link IntIntegerStruct}
		 */
		public abstract boolean lessThan(final IntIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '<'} equality for an {@link S} and an {@link LongIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than the provided {@link LongIntegerStruct}
		 */
		public abstract boolean lessThan(final LongIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '<'} equality for an {@link S} and an {@link BigIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than the provided {@link BigIntegerStruct}
		 */
		public abstract boolean lessThan(final BigIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '<'} equality for an {@link S} and an {@link SingleFloatStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than the provided {@link SingleFloatStruct}
		 */
		public abstract boolean lessThan(final SingleFloatStruct real2);

		/**
		 * Computes the numeric {@literal '<'} equality for an {@link S} and an {@link DoubleFloatStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than the provided {@link DoubleFloatStruct}
		 */
		public abstract boolean lessThan(final DoubleFloatStruct real2);

		/**
		 * Computes the numeric {@literal '<'} equality for an {@link S} and an {@link RatioStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than the provided {@link RatioStruct}
		 */
		public abstract boolean lessThan(final RatioStruct real2);
	}

	/**
	 * {@link GreaterThanVisitor} for computing numeric {@literal '>'} equality for {@link RealStruct}s.
	 */
	abstract class GreaterThanVisitor<S extends RealStruct> {

		/**
		 * The {@link S} as the first argument in the equality operation.
		 */
		final S real1;

		/**
		 * Package private constructor.
		 *
		 * @param real1
		 * 		the first argument in the equality operation
		 */
		GreaterThanVisitor(final S real1) {
			this.real1 = real1;
		}

		/**
		 * Computes the numeric {@literal '>'} equality for an {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than the provided {@link IntIntegerStruct}
		 */
		public abstract boolean greaterThan(final IntIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '>'} equality for an {@link S} and an {@link LongIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than the provided {@link LongIntegerStruct}
		 */
		public abstract boolean greaterThan(final LongIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '>'} equality for an {@link S} and an {@link BigIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than the provided {@link BigIntegerStruct}
		 */
		public abstract boolean greaterThan(final BigIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '>'} equality for an {@link S} and an {@link SingleFloatStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than the provided {@link SingleFloatStruct}
		 */
		public abstract boolean greaterThan(final SingleFloatStruct real2);

		/**
		 * Computes the numeric {@literal '>'} equality for an {@link S} and an {@link DoubleFloatStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than the provided {@link DoubleFloatStruct}
		 */
		public abstract boolean greaterThan(final DoubleFloatStruct real2);

		/**
		 * Computes the numeric {@literal '>'} equality for an {@link S} and an {@link RatioStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than the provided {@link RatioStruct}
		 */
		public abstract boolean greaterThan(final RatioStruct real2);
	}

	/**
	 * {@link LessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality for {@link RealStruct}s.
	 */
	abstract class LessThanOrEqualToVisitor<S extends RealStruct> {

		/**
		 * The {@link S} as the first argument in the equality operation.
		 */
		final S real1;

		/**
		 * Package private constructor.
		 *
		 * @param real1
		 * 		the first argument in the equality operation
		 */
		LessThanOrEqualToVisitor(final S real1) {
			this.real1 = real1;
		}

		/**
		 * Computes the numeric {@literal '<='} equality for an {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than or equal to the provided {@link IntIntegerStruct}
		 */
		public abstract boolean lessThanOrEqualTo(final IntIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '<='} equality for an {@link S} and an {@link LongIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than or equal to the provided {@link LongIntegerStruct}
		 */
		public abstract boolean lessThanOrEqualTo(final LongIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '<='} equality for an {@link S} and an {@link BigIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than or equal to the provided {@link BigIntegerStruct}
		 */
		public abstract boolean lessThanOrEqualTo(final BigIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '<='} equality for an {@link S} and an {@link SingleFloatStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than or equal to the provided {@link SingleFloatStruct}
		 */
		public abstract boolean lessThanOrEqualTo(final SingleFloatStruct real2);

		/**
		 * Computes the numeric {@literal '<='} equality for an {@link S} and an {@link DoubleFloatStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than or equal to the provided {@link DoubleFloatStruct}
		 */
		public abstract boolean lessThanOrEqualTo(final DoubleFloatStruct real2);

		/**
		 * Computes the numeric {@literal '<='} equality for an {@link S} and an {@link RatioStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is less than or equal to the provided {@link RatioStruct}
		 */
		public abstract boolean lessThanOrEqualTo(final RatioStruct real2);
	}

	/**
	 * {@link GreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality for {@link RealStruct}s.
	 */
	abstract class GreaterThanOrEqualToVisitor<S extends RealStruct> {

		/**
		 * The {@link S} as the first argument in the equality operation.
		 */
		final S real1;

		/**
		 * Package private constructor.
		 *
		 * @param real1
		 * 		the first argument in the equality operation
		 */
		GreaterThanOrEqualToVisitor(final S real1) {
			this.real1 = real1;
		}

		/**
		 * Computes the numeric {@literal '>='} equality for an {@link S} and an {@link IntIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than or equal to the provided {@link IntIntegerStruct}
		 */
		public abstract boolean greaterThanOrEqualTo(final IntIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '>='} equality for an {@link S} and an {@link LongIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than or equal to the provided {@link LongIntegerStruct}
		 */
		public abstract boolean greaterThanOrEqualTo(final LongIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '>='} equality for an {@link S} and an {@link BigIntegerStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than or equal to the provided {@link BigIntegerStruct}
		 */
		public abstract boolean greaterThanOrEqualTo(final BigIntegerStruct real2);

		/**
		 * Computes the numeric {@literal '>='} equality for an {@link S} and an {@link SingleFloatStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than or equal to the provided {@link SingleFloatStruct}
		 */
		public abstract boolean greaterThanOrEqualTo(final SingleFloatStruct real2);

		/**
		 * Computes the numeric {@literal '>='} equality for an {@link S} and an {@link DoubleFloatStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than or equal to the provided {@link DoubleFloatStruct}
		 */
		public abstract boolean greaterThanOrEqualTo(final DoubleFloatStruct real2);

		/**
		 * Computes the numeric {@literal '>='} equality for an {@link S} and an {@link RatioStruct}.
		 *
		 * @param real2
		 * 		the second argument in the equality operation
		 *
		 * @return true if {@link #real1} is greater than or equal to the provided {@link RatioStruct}
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

		public QuotientRemainderResult truncate(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.DOWN, false);
		}

		public QuotientRemainderResult truncate(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.DOWN, false);
		}

		public QuotientRemainderResult truncate(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.DOWN, false);
		}

		public QuotientRemainderResult ftruncate(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.DOWN, true);
		}

		public QuotientRemainderResult ftruncate(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.DOWN, true);
		}

		public QuotientRemainderResult ftruncate(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.DOWN, true);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the quotient and remainder results for a {@link RatioStruct} as the {@code real} and an {@link
		 * IntegerStruct} as the {@code divisor}.
		 */
		public abstract QuotientRemainderResult quotientRemainder(final IntegerStruct divisor, final RoundingMode roundingMode,
		                                                          final boolean isQuotientFloat);

		public abstract QuotientRemainderResult quotientRemainder(final FloatStruct divisor, final RoundingMode roundingMode,
		                                                          final boolean isQuotientFloat);

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

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import java.util.List;

import org.apfloat.Apfloat;
import org.apfloat.Aprational;

/**
 * The {@link RealStruct2} is the object representation of a Lisp 'real' type.
 */
public interface RealStruct2 extends NumberStruct2 {

	/**
	 * Performs a {@literal '<'} comparison of this RealStruct and the provided RealStruct.
	 *
	 * @param real
	 * 		the RealStruct to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of this RealStruct and the provided RealStruct
	 */
	boolean isLessThan(final RealStruct2 real);

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
	static boolean isLessThan(final RealStruct2 real, final List<RealStruct2> reals) {
		RealStruct2 previousReal = real;

		boolean result = true;
		for (final RealStruct2 currentReal : reals) {
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
	boolean isGreaterThan(final RealStruct2 real);

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
	static boolean isGreaterThan(final RealStruct2 real, final List<RealStruct2> reals) {
		RealStruct2 previousReal = real;

		boolean result = true;
		for (final RealStruct2 currentReal : reals) {
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
	boolean isLessThanOrEqualTo(final RealStruct2 real);

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
	static boolean isLessThanOrEqualTo(final RealStruct2 real, final List<RealStruct2> reals) {
		RealStruct2 previousReal = real;

		boolean result = true;
		for (final RealStruct2 currentReal : reals) {
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
	boolean isGreaterThanOrEqualTo(final RealStruct2 real);

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
	static boolean isGreaterThanOrEqualTo(final RealStruct2 real, final List<RealStruct2> reals) {
		RealStruct2 previousReal = real;

		boolean result = true;
		for (final RealStruct2 currentReal : reals) {
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
	default RealStruct2 max(final RealStruct2 real) {
		// TODO: better internally computed by Apfloat library???
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
	static RealStruct2 max(final RealStruct2 real, final List<RealStruct2> reals) {
		if (reals.isEmpty()) {
			return real;
		}
		return reals.stream().reduce(real, RealStruct2::max);
	}

	/**
	 * Returns the least positive value when comparing the value of this RealStruct and the provided RealStruct.
	 *
	 * @param real
	 * 		the RealStruct used and possibly returned if it is less than this RealStruct
	 *
	 * @return the least positive value when comparing the value of this RealStruct and the provided RealStruct
	 */
	default RealStruct2 min(final RealStruct2 real) {
		// TODO: better internally computed by Apfloat library???
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
	static RealStruct2 min(final RealStruct2 real, final List<RealStruct2> reals) {
		if (reals.isEmpty()) {
			return real;
		}
		return reals.stream().reduce(real, RealStruct2::min);
	}

	/**
	 * Returns this RealStruct as a representational {@link RationalStruct2}.
	 *
	 * @return this RealStruct as a representational {@link RationalStruct2}
	 */
	RationalStruct2 rational();

	/**
	 * Returns this RealStruct as a representational {@link FloatStruct2}.
	 *
	 * @return this RealStruct as a representational {@link FloatStruct2}
	 */
	FloatStruct2 floatingPoint();

	/**
	 * Returns this RealStruct as a representational {@link FloatStruct2}, using the prototype as the type of {@link
	 * FloatStruct2} to return.
	 *
	 * @param prototype
	 * 		an object representing the type of {@link FloatStruct2} to return
	 *
	 * @return this RealStruct as a representational {@link FloatStruct2}
	 */
	FloatStruct2 floatingPoint(final FloatStruct2 prototype);

	default RealStruct2 mod(final RealStruct2 divisor) {
//		final QuotientRemainderResult2 floor = floor(divisor);
//		return floor.getRemainder();
		return null;
	}

	default RealStruct2 rem(final RealStruct2 divisor) {
//		final QuotientRemainderResult2 truncate = truncate(divisor);
//		return truncate.getRemainder();
		return null;
	}

	default ComplexStruct2 cis() {
		return new ComplexStruct2(cos(), sin());
	}

	default QuotientRemainderResult2 floor() {
		// TODO: better internally computed by Apfloat library???
		return floor(IntegerStruct2.ONE);
	}

	QuotientRemainderResult2 floor(final RealStruct2 divisor);

	default QuotientRemainderResult2 ffloor() {
		// TODO: better internally computed by Apfloat library???
		return ffloor(IntegerStruct2.ONE);
	}

	QuotientRemainderResult2 ffloor(final RealStruct2 divisor);

	default QuotientRemainderResult2 ceiling() {
		// TODO: better internally computed by Apfloat library???
		return ceiling(IntegerStruct2.ONE);
	}

	QuotientRemainderResult2 ceiling(final RealStruct2 divisor);

	default QuotientRemainderResult2 fceiling() {
		// TODO: better internally computed by Apfloat library???
		return fceiling(IntegerStruct2.ONE);
	}

	QuotientRemainderResult2 fceiling(final RealStruct2 divisor);

	default QuotientRemainderResult2 truncate() {
		// TODO: better internally computed by Apfloat library???
		return truncate(IntegerStruct2.ONE);
	}

	QuotientRemainderResult2 truncate(final RealStruct2 divisor);

	default QuotientRemainderResult2 ftruncate() {
		// TODO: better internally computed by Apfloat library???
		return ftruncate(IntegerStruct2.ONE);
	}

	QuotientRemainderResult2 ftruncate(final RealStruct2 divisor);

	default QuotientRemainderResult2 round() {
		// TODO: better internally computed by Apfloat library???
		return round(IntegerStruct2.ONE);
	}

	QuotientRemainderResult2 round(final RealStruct2 divisor);

	default QuotientRemainderResult2 fround() {
		// TODO: better internally computed by Apfloat library???
		return fround(IntegerStruct2.ONE);
	}

	QuotientRemainderResult2 fround(final RealStruct2 divisor);

	/*
		NumberStruct
	 */

	@Override
	Apfloat ap();

	@Override
	default RealStruct2 realPart() {
		return this;
	}

	@Override
	default RealStruct2 conjugate() {
		return this;
	}

	@Override
	RealStruct2 sin();

	@Override
	RealStruct2 cos();

	@Override
	RealStruct2 tan();

	@Override
	RealStruct2 asin();

	@Override
	RealStruct2 acos();

	@Override
	RealStruct2 atan();

	RealStruct2 atan(RealStruct2 real);

	@Override
	RealStruct2 sinh();

	@Override
	RealStruct2 cosh();

	@Override
	RealStruct2 tanh();

	@Override
	RealStruct2 asinh();

	@Override
	RealStruct2 acosh();

	@Override
	RealStruct2 atanh();

	static RealStruct2 valueOf(final Apfloat apfloat) {
		if (apfloat instanceof Aprational) {
			return RationalStruct2.valueOf((Aprational) apfloat);
		}
		return FloatStruct2.valueOf(apfloat);
	}
}

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
	 * Returns a RealStruct2 from the provided {@link Apfloat} value. If the {@link Apfloat} is an {@link Aprational},
	 * {@link RationalStruct2#valueOf(Aprational)} is invoked to create the appropriate {@link RationalStruct2} instead.
	 *
	 * @param apfloat
	 * 		the {@link Apfloat} to be used as the value of the resulting RealStruct2
	 *
	 * @return a RealStruct2 with the provided {@link Apfloat} as its value
	 */
	static RealStruct2 valueOf(final Apfloat apfloat) {
		if (apfloat instanceof Aprational) {
			return RationalStruct2.valueOf((Aprational) apfloat);
		}
		return FloatStruct2.valueOf(apfloat);
	}

	/**
	 * Performs a {@literal '<'} comparison of this RealStruct2 and the provided RealStruct2.
	 *
	 * @param real
	 * 		the RealStruct2 to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of this RealStruct2 and the provided RealStruct2
	 */
	boolean isLessThan(RealStruct2 real);

	/**
	 * Performs a {@literal '<'} comparison of the provided RealStruct2 objects in order, using the single RealStruct2
	 * as the starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct2 used in the {@literal '<'} comparison
	 * @param reals
	 * 		the {@link List} of RealStruct2 objects used in the {@literal '<'} comparison
	 *
	 * @return the {@literal '<'} comparison provided RealStruct2 objects
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
	 * Performs a {@literal '>'} comparison of this RealStruct2 and the provided RealStruct2.
	 *
	 * @param real
	 * 		the RealStruct2 to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of this RealStruct2 and the provided RealStruct2
	 */
	boolean isGreaterThan(RealStruct2 real);

	/**
	 * Performs a {@literal '>'} comparison of the provided RealStruct2 objects in order, using the single RealStruct2
	 * as the starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct2 used in the {@literal '>'} comparison
	 * @param reals
	 * 		the {@link List} of RealStruct2 objects used in the {@literal '>'} comparison
	 *
	 * @return the {@literal '>'} comparison provided RealStruct2 objects
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
	 * Performs a {@literal '<='} comparison of this RealStruct2 and the provided RealStruct2.
	 *
	 * @param real
	 * 		the RealStruct2 to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of this RealStruct2 and the provided RealStruct2
	 */
	boolean isLessThanOrEqualTo(RealStruct2 real);

	/**
	 * Performs a {@literal '<='} comparison of the provided RealStruct2 objects in order, using the single RealStruct2
	 * as the starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct2 used in the {@literal '<='} comparison
	 * @param reals
	 * 		the {@link List} of RealStruct2 objects used in the {@literal '<='} comparison
	 *
	 * @return the {@literal '<='} comparison provided RealStruct2 objects
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
	 * Performs a {@literal '>='} comparison of this RealStruct2 and the provided RealStruct2.
	 *
	 * @param real
	 * 		the RealStruct2 to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of this RealStruct2 and the provided RealStruct2
	 */
	boolean isGreaterThanOrEqualTo(RealStruct2 real);

	/**
	 * Performs a {@literal '>='} comparison of the provided RealStruct2 objects in order, using the single RealStruct2
	 * as the starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct2 used in the {@literal '>='} comparison
	 * @param reals
	 * 		the {@link List} of RealStruct2 objects used in the {@literal '>='} comparison
	 *
	 * @return the {@literal '>='} comparison provided RealStruct2 objects
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
	 * Returns {@code true} if this RealStruct2 is positive; false otherwise.
	 *
	 * @return {@code true} if this RealStruct2 is positive; false otherwise
	 */
	boolean plusp();

	/**
	 * Returns {@code true} if this RealStruct2 is negative; false otherwise.
	 *
	 * @return {@code true} if this RealStruct2 is negative; false otherwise
	 */
	boolean minusp();

	/**
	 * Returns the most positive value when comparing the value of this RealStruct2 and the provided RealStruct2.
	 *
	 * @param real
	 * 		the RealStruct2 used and possibly returned if it is greater than this RealStruct2
	 *
	 * @return the most positive value when comparing the value of this RealStruct2 and the provided RealStruct2
	 */
	default RealStruct2 max(final RealStruct2 real) {
		return isGreaterThanOrEqualTo(real) ? this : real;
	}

	/**
	 * Returns the most positive value when comparing the values of the provided RealStruct2 objects in order, using the
	 * single RealStruct2 as the starting point in the comparison.
	 *
	 * @param real
	 * 		the initial RealStruct2 used in the comparison, and possibly returned if the {@link List} of RealStruct2
	 * 		objects is empty
	 * @param reals
	 * 		the {@link List} of RealStruct2 objects used in the comparison in determining the greatest value
	 *
	 * @return the most positive value when comparing the value of the initial RealStruct2 and the provided {@link List}
	 * of RealStruct2 objects
	 */
	static RealStruct2 max(final RealStruct2 real, final List<RealStruct2> reals) {
		if (reals.isEmpty()) {
			return real;
		}
		return reals.stream().reduce(real, RealStruct2::max);
	}

	/**
	 * Returns the least positive value when comparing the value of this RealStruct2 and the provided RealStruct2.
	 *
	 * @param real
	 * 		the RealStruct2 used and possibly returned if it is less than this RealStruct2
	 *
	 * @return the least positive value when comparing the value of this RealStruct2 and the provided RealStruct2
	 */
	default RealStruct2 min(final RealStruct2 real) {
		return isLessThanOrEqualTo(real) ? this : real;
	}

	/**
	 * Returns the least positive value when comparing the values of the provided RealStruct2 objects in order, using
	 * the single RealStruct2 as the starting point in the comparison.
	 *
	 * @param real
	 * 		the initial RealStruct2 used in the comparison, and possibly returned if the {@link List} of RealStruct2
	 * 		objects is empty
	 * @param reals
	 * 		the {@link List} of RealStruct2 objects used in the comparison in determining the least value
	 *
	 * @return the least positive value when comparing the value of the initial RealStruct2 and the provided {@link
	 * List} of RealStruct2 objects
	 */
	static RealStruct2 min(final RealStruct2 real, final List<RealStruct2> reals) {
		if (reals.isEmpty()) {
			return real;
		}
		return reals.stream().reduce(real, RealStruct2::min);
	}

	/**
	 * Returns this RealStruct2 as a representational {@link RationalStruct2}.
	 *
	 * @return this RealStruct2 as a representational {@link RationalStruct2}
	 */
	RationalStruct2 rational();

	/**
	 * Returns this RealStruct2 as a representational {@link FloatStruct2}.
	 *
	 * @return this RealStruct2 as a representational {@link FloatStruct2}
	 */
	FloatStruct2 floatingPoint();

	/**
	 * Returns this RealStruct2 as a representational {@link FloatStruct2}, using the prototype as the precision
	 * representation for the {@link FloatStruct2} to return.
	 *
	 * @param prototype
	 * 		an object representing the precision of {@link FloatStruct2} to return
	 *
	 * @return this RealStruct2 as a representational {@link FloatStruct2}
	 */
	FloatStruct2 floatingPoint(FloatStruct2 prototype);

	/**
	 * Returns the modulus of this RealStruct2 and the provided RealStruct2 divisor.
	 *
	 * @param divisor
	 * 		the RealStruct2 to use as the divisor in the modulus operation
	 *
	 * @return the modulus of this RealStruct2 and the provided RealStruct2 divisor
	 */
	default RealStruct2 mod(final RealStruct2 divisor) {
		final QuotientRemainder2 floor = floor(divisor);
		return floor.getRemainder();
	}

	/**
	 * Returns the remainder of this RealStruct2 and the provided RealStruct2 divisor.
	 *
	 * @param divisor
	 * 		the RealStruct2 to use as the divisor in the remainder operation
	 *
	 * @return the remainder of this RealStruct2 and the provided RealStruct2 divisor
	 */
	default RealStruct2 rem(final RealStruct2 divisor) {
		final QuotientRemainder2 truncate = truncate(divisor);
		return truncate.getRemainder();
	}

	/**
	 * Returns the value of e^i* radians, which is a complex in which the real part is equal to the cosine of radians,
	 * and the imaginary part is equal to the sine of radians.
	 *
	 * @return the value of e^i* radians
	 */
	default ComplexStruct2 cis() {
		final RealStruct2 cos = cos();
		final RealStruct2 sin = sin();
		return ComplexStruct2.valueOf(cos.ap(), sin.ap());
	}

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'FLOOR' operation for this RealStruct2.
	 *
	 * @return the {@link QuotientRemainder2} for the 'FLOOR' operation
	 */
	QuotientRemainder2 floor();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'FLOOR' operation for this RealStruct2 with the provided
	 * RealStruct2 divisor.
	 *
	 * @param divisor
	 * 		the RealStruct2 to be used to be used as the divisor in the 'FLOOR' operation
	 *
	 * @return the {@link QuotientRemainder2} for the 'FLOOR' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 floor(RealStruct2 divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'FLOOR' operation for this RealStruct2. The resulting
	 * 'quotient' will be a {@link FloatStruct2}.
	 *
	 * @return the {@link QuotientRemainder2} for the 'FLOOR' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 ffloor();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'FLOOR' operation for this RealStruct2 with the provided
	 * RealStruct2 divisor. The resulting 'quotient' will be a {@link FloatStruct2}.
	 *
	 * @param divisor
	 * 		the RealStruct2 to be used to be used as the divisor in the 'FLOOR' operation
	 *
	 * @return the {@link QuotientRemainder2} for the 'FLOOR' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 ffloor(RealStruct2 divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'CEILING' operation for this RealStruct2.
	 *
	 * @return the {@link QuotientRemainder2} for the 'CEILING' operation
	 */
	QuotientRemainder2 ceiling();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'CEILING' operation for this RealStruct2 with the provided
	 * RealStruct2 divisor.
	 *
	 * @param divisor
	 * 		the RealStruct2 to be used to be used as the divisor in the 'CEILING' operation
	 *
	 * @return the {@link QuotientRemainder2} for the 'CEILING' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 ceiling(RealStruct2 divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'CEILING' operation for this RealStruct2. The resulting
	 * 'quotient' will be a {@link FloatStruct2}.
	 *
	 * @return the {@link QuotientRemainder2} for the 'CEILING' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 fceiling();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'CEILING' operation for this RealStruct2 with the provided
	 * RealStruct2 divisor. The resulting 'quotient' will be a {@link FloatStruct2}.
	 *
	 * @param divisor
	 * 		the RealStruct2 to be used to be used as the divisor in the 'CEILING' operation
	 *
	 * @return the {@link QuotientRemainder2} for the 'CEILING' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 fceiling(RealStruct2 divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'TRUNCATE' operation for this RealStruct2.
	 *
	 * @return the {@link QuotientRemainder2} for the 'TRUNCATE' operation
	 */
	QuotientRemainder2 truncate();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'TRUNCATE' operation for this RealStruct2 with the provided
	 * RealStruct2 divisor.
	 *
	 * @param divisor
	 * 		the RealStruct2 to be used to be used as the divisor in the 'TRUNCATE' operation
	 *
	 * @return the {@link QuotientRemainder2} for the 'TRUNCATE' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 truncate(RealStruct2 divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'TRUNCATE' operation for this RealStruct2. The resulting
	 * 'quotient' will be a {@link FloatStruct2}.
	 *
	 * @return the {@link QuotientRemainder2} for the 'TRUNCATE' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 ftruncate();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'TRUNCATE' operation for this RealStruct2 with the provided
	 * RealStruct2 divisor. The resulting 'quotient' will be a {@link FloatStruct2}.
	 *
	 * @param divisor
	 * 		the RealStruct2 to be used to be used as the divisor in the 'TRUNCATE' operation
	 *
	 * @return the {@link QuotientRemainder2} for the 'TRUNCATE' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 ftruncate(RealStruct2 divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'ROUND' operation for this RealStruct2.
	 *
	 * @return the {@link QuotientRemainder2} for the 'ROUND' operation
	 */
	QuotientRemainder2 round();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'ROUND' operation for this RealStruct2 with the provided
	 * RealStruct2 divisor.
	 *
	 * @param divisor
	 * 		the RealStruct2 to be used to be used as the divisor in the 'ROUND' operation
	 *
	 * @return the {@link QuotientRemainder2} for the 'ROUND' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 round(RealStruct2 divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'ROUND' operation for this RealStruct2. The resulting
	 * 'quotient' will be a {@link FloatStruct2}.
	 *
	 * @return the {@link QuotientRemainder2} for the 'ROUND' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 fround();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'ROUND' operation for this RealStruct2 with the provided
	 * RealStruct2 divisor. The resulting 'quotient' will be a {@link FloatStruct2}.
	 *
	 * @param divisor
	 * 		the RealStruct2 to be used to be used as the divisor in the 'ROUND' operation
	 *
	 * @return the {@link QuotientRemainder2} for the 'ROUND' operation with the provided RealStruct2 divisor
	 */
	QuotientRemainder2 fround(RealStruct2 divisor);

	/**
	 * Returns the 'Inverse tangent' of this RealStruct2 divided by the provided RealStruct2.
	 *
	 * @param real
	 * 		the RealStruct2 to divide this RealStruct2 by
	 *
	 * @return the 'Inverse tangent' of this RealStruct2 divided by the provided RealStruct2
	 */
	RealStruct2 atan(RealStruct2 real);

	/*
		NumberStruct
	 */

	@Override
	Apfloat ap();

	@Override
	RealStruct2 signum();

	@Override
	default RealStruct2 realPart() {
		return this;
	}

	@Override
	default RealStruct2 conjugate() {
		return this;
	}

	@Override
	RealStruct2 negation();

	@Override
	RealStruct2 reciprocal();

	@Override
	RealStruct2 exp();

	@Override
	RealStruct2 log();

	@Override
	RealStruct2 sqrt();

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
}

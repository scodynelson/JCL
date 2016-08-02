/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.util.List;

import jcl.lang.number.ComplexStructImpl;
import jcl.lang.number.FloatStructImpl;
import jcl.lang.number.QuotientRemainder;
import org.apfloat.Apfloat;
import org.apfloat.Aprational;

/**
 * The {@link RealStruct} is the object representation of a Lisp 'real' type.
 */
public interface RealStruct extends NumberStruct {

	/**
	 * Returns a RealStruct from the provided {@link Apfloat} value. If the {@link Apfloat} is an {@link Aprational},
	 * {@link RationalStruct#valueOf(Aprational)} is invoked to create the appropriate {@link RationalStruct} instead.
	 *
	 * @param apfloat
	 * 		the {@link Apfloat} to be used as the value of the resulting RealStruct
	 *
	 * @return a RealStruct with the provided {@link Apfloat} as its value
	 */
	static RealStruct valueOf(final Apfloat apfloat) {
		if (apfloat instanceof Aprational) {
			return RationalStruct.valueOf((Aprational) apfloat);
		}
		return FloatStructImpl.valueOf(apfloat);
	}

	/**
	 * Performs a {@literal '<'} comparison of this RealStruct and the provided RealStruct.
	 *
	 * @param real
	 * 		the RealStruct to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of this RealStruct and the provided RealStruct
	 */
	boolean isLessThan(RealStruct real);

	/**
	 * Performs a {@literal '<'} comparison of the provided RealStruct objects in order, using the single RealStruct
	 * as the starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct used in the {@literal '<'} comparison
	 * @param reals
	 * 		the {@link List} of RealStruct objects used in the {@literal '<'} comparison
	 *
	 * @return the {@literal '<'} comparison provided RealStruct objects
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
	boolean isGreaterThan(RealStruct real);

	/**
	 * Performs a {@literal '>'} comparison of the provided RealStruct objects in order, using the single RealStruct
	 * as the starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct used in the {@literal '>'} comparison
	 * @param reals
	 * 		the {@link List} of RealStruct objects used in the {@literal '>'} comparison
	 *
	 * @return the {@literal '>'} comparison provided RealStruct objects
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
	boolean isLessThanOrEqualTo(RealStruct real);

	/**
	 * Performs a {@literal '<='} comparison of the provided RealStruct objects in order, using the single RealStruct
	 * as the starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct used in the {@literal '<='} comparison
	 * @param reals
	 * 		the {@link List} of RealStruct objects used in the {@literal '<='} comparison
	 *
	 * @return the {@literal '<='} comparison provided RealStruct objects
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
	boolean isGreaterThanOrEqualTo(RealStruct real);

	/**
	 * Performs a {@literal '>='} comparison of the provided RealStruct objects in order, using the single RealStruct
	 * as the starting point in the comparison. If at any point a value does not follow the expected comparison, the
	 * comparison loop with short-circuit.
	 *
	 * @param real
	 * 		the initial RealStruct used in the {@literal '>='} comparison
	 * @param reals
	 * 		the {@link List} of RealStruct objects used in the {@literal '>='} comparison
	 *
	 * @return the {@literal '>='} comparison provided RealStruct objects
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
	 * Returns {@code true} if this RealStruct is positive; false otherwise.
	 *
	 * @return {@code true} if this RealStruct is positive; false otherwise
	 */
	boolean plusp();

	/**
	 * Returns {@code true} if this RealStruct is negative; false otherwise.
	 *
	 * @return {@code true} if this RealStruct is negative; false otherwise
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
	 * Returns the most positive value when comparing the values of the provided RealStruct objects in order, using the
	 * single RealStruct as the starting point in the comparison.
	 *
	 * @param real
	 * 		the initial RealStruct used in the comparison, and possibly returned if the {@link List} of RealStruct
	 * 		objects is empty
	 * @param reals
	 * 		the {@link List} of RealStruct objects used in the comparison in determining the greatest value
	 *
	 * @return the most positive value when comparing the value of the initial RealStruct and the provided {@link List}
	 * of RealStruct objects
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
	 * Returns the least positive value when comparing the values of the provided RealStruct objects in order, using
	 * the single RealStruct as the starting point in the comparison.
	 *
	 * @param real
	 * 		the initial RealStruct used in the comparison, and possibly returned if the {@link List} of RealStruct
	 * 		objects is empty
	 * @param reals
	 * 		the {@link List} of RealStruct objects used in the comparison in determining the least value
	 *
	 * @return the least positive value when comparing the value of the initial RealStruct and the provided {@link
	 * List} of RealStruct objects
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
	 * Returns this RealStruct as a representational {@link FloatStructImpl}.
	 *
	 * @return this RealStruct as a representational {@link FloatStructImpl}
	 */
	FloatStructImpl floatingPoint();

	/**
	 * Returns this RealStruct as a representational {@link FloatStructImpl}, using the prototype as the precision
	 * representation for the {@link FloatStructImpl} to return.
	 *
	 * @param prototype
	 * 		an object representing the precision of {@link FloatStructImpl} to return
	 *
	 * @return this RealStruct as a representational {@link FloatStructImpl}
	 */
	FloatStructImpl floatingPoint(FloatStructImpl prototype);

	/**
	 * Returns the modulus of this RealStruct and the provided RealStruct divisor.
	 *
	 * @param divisor
	 * 		the RealStruct to use as the divisor in the modulus operation
	 *
	 * @return the modulus of this RealStruct and the provided RealStruct divisor
	 */
	default RealStruct mod(final RealStruct divisor) {
		final QuotientRemainder floor = floor(divisor);
		return floor.getRemainder();
	}

	/**
	 * Returns the remainder of this RealStruct and the provided RealStruct divisor.
	 *
	 * @param divisor
	 * 		the RealStruct to use as the divisor in the remainder operation
	 *
	 * @return the remainder of this RealStruct and the provided RealStruct divisor
	 */
	default RealStruct rem(final RealStruct divisor) {
		final QuotientRemainder truncate = truncate(divisor);
		return truncate.getRemainder();
	}

	/**
	 * Returns the value of e^i* radians, which is a complex in which the real part is equal to the cosine of radians,
	 * and the imaginary part is equal to the sine of radians.
	 *
	 * @return the value of e^i* radians
	 */
	default ComplexStructImpl cis() {
		final RealStruct cos = cos();
		final RealStruct sin = sin();
		// TODO
		return ComplexStructImpl.valueOf(cos.ap(), sin.ap(), ComplexStructImpl.ValueType.RATIONAL);
	}

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'FLOOR' operation for this RealStruct.
	 *
	 * @return the {@link QuotientRemainder} for the 'FLOOR' operation
	 */
	QuotientRemainder floor();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'FLOOR' operation for this RealStruct with the provided
	 * RealStruct divisor.
	 *
	 * @param divisor
	 * 		the RealStruct to be used to be used as the divisor in the 'FLOOR' operation
	 *
	 * @return the {@link QuotientRemainder} for the 'FLOOR' operation with the provided RealStruct divisor
	 */
	QuotientRemainder floor(RealStruct divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'FLOOR' operation for this RealStruct. The resulting
	 * 'quotient' will be a {@link FloatStructImpl}.
	 *
	 * @return the {@link QuotientRemainder} for the 'FLOOR' operation with the provided RealStruct divisor
	 */
	QuotientRemainder ffloor();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'FLOOR' operation for this RealStruct with the provided
	 * RealStruct divisor. The resulting 'quotient' will be a {@link FloatStructImpl}.
	 *
	 * @param divisor
	 * 		the RealStruct to be used to be used as the divisor in the 'FLOOR' operation
	 *
	 * @return the {@link QuotientRemainder} for the 'FLOOR' operation with the provided RealStruct divisor
	 */
	QuotientRemainder ffloor(RealStruct divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'CEILING' operation for this RealStruct.
	 *
	 * @return the {@link QuotientRemainder} for the 'CEILING' operation
	 */
	QuotientRemainder ceiling();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'CEILING' operation for this RealStruct with the provided
	 * RealStruct divisor.
	 *
	 * @param divisor
	 * 		the RealStruct to be used to be used as the divisor in the 'CEILING' operation
	 *
	 * @return the {@link QuotientRemainder} for the 'CEILING' operation with the provided RealStruct divisor
	 */
	QuotientRemainder ceiling(RealStruct divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'CEILING' operation for this RealStruct. The resulting
	 * 'quotient' will be a {@link FloatStructImpl}.
	 *
	 * @return the {@link QuotientRemainder} for the 'CEILING' operation with the provided RealStruct divisor
	 */
	QuotientRemainder fceiling();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'CEILING' operation for this RealStruct with the provided
	 * RealStruct divisor. The resulting 'quotient' will be a {@link FloatStructImpl}.
	 *
	 * @param divisor
	 * 		the RealStruct to be used to be used as the divisor in the 'CEILING' operation
	 *
	 * @return the {@link QuotientRemainder} for the 'CEILING' operation with the provided RealStruct divisor
	 */
	QuotientRemainder fceiling(RealStruct divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'TRUNCATE' operation for this RealStruct.
	 *
	 * @return the {@link QuotientRemainder} for the 'TRUNCATE' operation
	 */
	QuotientRemainder truncate();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'TRUNCATE' operation for this RealStruct with the provided
	 * RealStruct divisor.
	 *
	 * @param divisor
	 * 		the RealStruct to be used to be used as the divisor in the 'TRUNCATE' operation
	 *
	 * @return the {@link QuotientRemainder} for the 'TRUNCATE' operation with the provided RealStruct divisor
	 */
	QuotientRemainder truncate(RealStruct divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'TRUNCATE' operation for this RealStruct. The resulting
	 * 'quotient' will be a {@link FloatStructImpl}.
	 *
	 * @return the {@link QuotientRemainder} for the 'TRUNCATE' operation with the provided RealStruct divisor
	 */
	QuotientRemainder ftruncate();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'TRUNCATE' operation for this RealStruct with the provided
	 * RealStruct divisor. The resulting 'quotient' will be a {@link FloatStructImpl}.
	 *
	 * @param divisor
	 * 		the RealStruct to be used to be used as the divisor in the 'TRUNCATE' operation
	 *
	 * @return the {@link QuotientRemainder} for the 'TRUNCATE' operation with the provided RealStruct divisor
	 */
	QuotientRemainder ftruncate(RealStruct divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'ROUND' operation for this RealStruct.
	 *
	 * @return the {@link QuotientRemainder} for the 'ROUND' operation
	 */
	QuotientRemainder round();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'ROUND' operation for this RealStruct with the provided
	 * RealStruct divisor.
	 *
	 * @param divisor
	 * 		the RealStruct to be used to be used as the divisor in the 'ROUND' operation
	 *
	 * @return the {@link QuotientRemainder} for the 'ROUND' operation with the provided RealStruct divisor
	 */
	QuotientRemainder round(RealStruct divisor);

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'ROUND' operation for this RealStruct. The resulting
	 * 'quotient' will be a {@link FloatStructImpl}.
	 *
	 * @return the {@link QuotientRemainder} for the 'ROUND' operation with the provided RealStruct divisor
	 */
	QuotientRemainder fround();

	/**
	 * Calculates the 'quotient' and 'remainder' for the 'ROUND' operation for this RealStruct with the provided
	 * RealStruct divisor. The resulting 'quotient' will be a {@link FloatStructImpl}.
	 *
	 * @param divisor
	 * 		the RealStruct to be used to be used as the divisor in the 'ROUND' operation
	 *
	 * @return the {@link QuotientRemainder} for the 'ROUND' operation with the provided RealStruct divisor
	 */
	QuotientRemainder fround(RealStruct divisor);

	/**
	 * Returns the 'Inverse tangent' of this RealStruct divided by the provided RealStruct.
	 *
	 * @param real
	 * 		the RealStruct to divide this RealStruct by
	 *
	 * @return the 'Inverse tangent' of this RealStruct divided by the provided RealStruct
	 */
	RealStruct atan(RealStruct real);

	/*
		NumberStruct
	 */

	@Override
	Apfloat ap();

	@Override
	RealStruct signum();

	@Override
	default RealStruct realPart() {
		return this;
	}

	@Override
	default RealStruct conjugate() {
		return this;
	}

	@Override
	RealStruct negation();

	@Override
	RealStruct reciprocal();

	@Override
	RealStruct exp();

	@Override
	RealStruct log();

	@Override
	RealStruct sqrt();

	@Override
	RealStruct sin();

	@Override
	RealStruct cos();

	@Override
	RealStruct tan();

	@Override
	RealStruct asin();

	@Override
	RealStruct acos();

	@Override
	RealStruct atan();

	@Override
	RealStruct sinh();

	@Override
	RealStruct cosh();

	@Override
	RealStruct tanh();

	@Override
	RealStruct asinh();

	@Override
	RealStruct acosh();

	@Override
	RealStruct atanh();
}

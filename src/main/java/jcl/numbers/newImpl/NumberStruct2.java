/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import java.util.List;

import jcl.LispStruct;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;

/**
 * The {@link NumberStruct2} is the object representation of a Lisp 'number' type.
 */
public interface NumberStruct2 extends LispStruct {

	/**
	 * Returns a NumberStruct2 from the provided {@link Apcomplex} value. If the {@link Apcomplex} is an {@link
	 * Apfloat}, {@link RealStruct2#valueOf(Apfloat)} is invoked to create the appropriate {@link RealStruct2} instead.
	 *
	 * @param apcomplex
	 * 		the {@link Apcomplex} to be used as the value of the resulting NumberStruct2
	 *
	 * @return a NumberStruct2 with the provided {@link Apcomplex} as its value
	 */
	static NumberStruct2 valueOf(final Apcomplex apcomplex) {
		if (apcomplex instanceof Apfloat) {
			return RealStruct2.valueOf((Apfloat) apcomplex);
		}
		// TODO
		return ComplexStruct2.valueOf(apcomplex, ComplexStruct2.ComplexValueType.INTEGER);
	}

	/**
	 * Returns the {@link Apcomplex} that is used for NumberStruct2 calculations.
	 *
	 * @return the {@link Apcomplex} that is used for NumberStruct2 calculations
	 */
	Apcomplex ap();

	/**
	 * Returns the absolute value of this NumberStruct2.
	 *
	 * @return the absolute value of this NumberStruct2
	 */
	RealStruct2 abs();

	/**
	 * Returns {@code true} if this NumberStruct2 is zero; false otherwise.
	 *
	 * @return {@code true} if this NumberStruct2 is zero; false otherwise
	 */
	boolean zerop();

	/**
	 * Performs the addition operation on this NumberStruct2 to the provided NumberStruct2.
	 *
	 * @param number
	 * 		the NumberStruct2 to use in the addition operation
	 *
	 * @return the result of the addition operation on this NumberStruct2 to the provided NumberStruct2
	 */
	NumberStruct2 add(NumberStruct2 number);

	/**
	 * Performs the addition operation on the {@link List} of NumberStruct2 objects in order. {@link
	 * IntegerStruct2#ZERO} is returned if the list is empty.
	 *
	 * @param numbers
	 * 		the {@link List} of NumberStruct2 objects to performs the addition operation on
	 *
	 * @return the result of the addition operation against the {@link List} of NumberStruct2 objects
	 */
	static NumberStruct2 add(final List<NumberStruct2> numbers) {
		return numbers.stream().reduce(IntegerStruct2.ZERO, NumberStruct2::add);
	}

	/**
	 * Performs the subtraction operation on the provided NumberStruct2 from this NumberStruct2.
	 *
	 * @param number
	 * 		the NumberStruct2 to use in the subtraction operation
	 *
	 * @return the result of the subtraction operation on the provided NumberStruct2 from this NumberStruct2
	 */
	NumberStruct2 subtract(NumberStruct2 number);

	/**
	 * Performs the subtraction operation on the {@link List} of NumberStruct2 objects in order, using the single
	 * provided NumberStruct2 as the starting point. The {@link #negation()} of the single provided NumberStruct2 is
	 * returned if the list is empty.
	 *
	 * @param number
	 * 		the single NumberStruct2 to use as the starting point of the subtraction operation, or as the base of the
	 * 		{@link #negation()} result if the {@link List} of NumberStruct2 objects is empty
	 * @param numbers
	 * 		the {@link List} of NumberStruct2 objects to performs the subtraction operation on
	 *
	 * @return the result of the subtraction operation against the {@link List} of NumberStruct2 objects
	 */
	static NumberStruct2 subtract(final NumberStruct2 number, final List<NumberStruct2> numbers) {
		if (numbers.isEmpty()) {
			return number.negation();
		}
		return numbers.stream().reduce(number, NumberStruct2::subtract);
	}

	/**
	 * Performs the multiplication operation on this NumberStruct2 to the provided NumberStruct2.
	 *
	 * @param number
	 * 		the NumberStruct2 to use in the multiplication operation
	 *
	 * @return the result of the multiplication operation on this NumberStruct2 to the provided NumberStruct2
	 */
	NumberStruct2 multiply(NumberStruct2 number);

	/**
	 * Performs the multiplication operation on the {@link List} of NumberStruct2 objects in order. {@link
	 * IntegerStruct2#ONE} is returned if the list is empty.
	 *
	 * @param numbers
	 * 		the {@link List} of NumberStruct2 objects to performs the multiplication operation on
	 *
	 * @return the result of the multiplication operation against the {@link List} of NumberStruct2 objects
	 */
	static NumberStruct2 multiply(final List<NumberStruct2> numbers) {
		return numbers.stream().reduce(IntegerStruct2.ONE, NumberStruct2::multiply);
	}

	/**
	 * Performs the division operation on the provided NumberStruct2 from this NumberStruct2.
	 *
	 * @param number
	 * 		the NumberStruct2 to use in the division operation
	 *
	 * @return the result of the division operation on the provided NumberStruct2 from this NumberStruct2
	 */
	NumberStruct2 divide(NumberStruct2 number);

	/**
	 * Performs the division operation on the {@link List} of NumberStruct2 objects in order, using the single
	 * provided NumberStruct2 as the starting point. The {@link #reciprocal()} of the single provided NumberStruct2 is
	 * returned if the list is empty.
	 *
	 * @param number
	 * 		the single NumberStruct2 to use as the starting point of the division operation, or as the base of the
	 * 		{@link #reciprocal()} result if the {@link List} of NumberStruct2 objects is empty
	 * @param numbers
	 * 		the {@link List} of NumberStruct2 objects to performs the division operation on
	 *
	 * @return the result of the division operation against the {@link List} of NumberStruct2 objects
	 */
	static NumberStruct2 divide(final NumberStruct2 number, final List<NumberStruct2> numbers) {
		if (numbers.isEmpty()) {
			return number.reciprocal();
		}
		return numbers.stream().reduce(number, NumberStruct2::divide);
	}

	/**
	 * Performs a {@literal '=='} comparison of this NumberStruct2 and the provided NumberStruct2.
	 *
	 * @param number
	 * 		the NumberStruct2 to be used in the {@literal '=='} operation
	 *
	 * @return the {@literal '=='} comparison of this NumberStruct2 and the provided NumberStruct2
	 */
	boolean isEqualTo(NumberStruct2 number);

	/**
	 * Performs a {@literal '=='} comparison of the provided NumberStruct2 objects in order, using the single
	 * NumberStruct2 as the starting point in the comparison. If at any point a value does not follow the expected
	 * comparison, the comparison loop with short-circuit.
	 *
	 * @param number
	 * 		the initial NumberStruct2 used in the {@literal '=='} comparison
	 * @param numbers
	 * 		the {@link List} of NumberStruct2 objects used in the {@literal '=='} comparison
	 *
	 * @return the {@literal '=='} comparison provided NumberStruct2 objects
	 */
	static boolean isEqualTo(final NumberStruct2 number, final List<NumberStruct2> numbers) {
		NumberStruct2 previousNumber = number;

		boolean result = true;
		for (final NumberStruct2 currentNumber : numbers) {
			result = previousNumber.isEqualTo(currentNumber);
			if (!result) {
				break;
			}
			previousNumber = currentNumber;
		}
		return result;
	}

	/**
	 * Performs a {@literal '!='} comparison of this NumberStruct2 and the provided NumberStruct2.
	 *
	 * @param number
	 * 		the NumberStruct2 to be used in the {@literal '!='} operation
	 *
	 * @return the {@literal '!='} comparison of this NumberStruct2 and the provided NumberStruct2
	 */
	default boolean isNotEqualTo(final NumberStruct2 number) {
		return !isEqualTo(number);
	}

	/**
	 * Performs a {@literal '!='} comparison of the provided NumberStruct2 objects in order, using the single
	 * NumberStruct2 as the starting point in the comparison. If at any point a value does not follow the expected
	 * comparison, the comparison loop with short-circuit.
	 *
	 * @param number
	 * 		the initial NumberStruct2 used in the {@literal '!='} comparison
	 * @param numbers
	 * 		the {@link List} of NumberStruct2 objects used in the {@literal '!='} comparison
	 *
	 * @return the {@literal '!='} comparison provided NumberStruct2 objects
	 */
	static boolean isNotEqualTo(final NumberStruct2 number, final List<NumberStruct2> numbers) {
		NumberStruct2 previousNumber = number;

		boolean result = true;
		for (final NumberStruct2 currentNumber : numbers) {
			result = previousNumber.isNotEqualTo(currentNumber);
			if (!result) {
				break;
			}
			previousNumber = currentNumber;
		}
		return result;
	}

	/**
	 * Determines the numerical values that indicates whether this NumberStruct2 is negative, zero, or positive.
	 *
	 * @return the numerical values that indicates whether this NumberStruct2 is negative, zero, or positive
	 */
	NumberStruct2 signum();

	/**
	 * Returns the 'real' part of this NumberStruct2.
	 *
	 * @return the 'real' part of this NumberStruct2
	 */
	RealStruct2 realPart();

	/**
	 * Returns the 'imaginary' part of this NumberStruct2.
	 *
	 * @return the 'imaginary' part of this NumberStruct2
	 */
	RealStruct2 imagPart();

	/**
	 * Returns the complex conjugate of this NumberStruct2.
	 *
	 * @return the complex conjugate of this NumberStruct2
	 */
	NumberStruct2 conjugate();

	/**
	 * Returns the negation of this NumberStruct2.
	 *
	 * @return the negation of this NumberStruct2
	 */
	NumberStruct2 negation();

	/**
	 * Returns the reciprocal of this NumberStruct2.
	 *
	 * @return the reciprocal of this NumberStruct2
	 */
	NumberStruct2 reciprocal();

	/**
	 * Returns 'e' raised to the power of this NumberStruct2, where 'e' is the base of the natural logarithms.
	 *
	 * @return 'e' raised to the power of this NumberStruct2
	 */
	NumberStruct2 exp();

	/**
	 * Returns the result of this NumberStruct2 raised to the power of the provided NumberStruct2.
	 *
	 * @param power
	 * 		the NumberStruct to use as the power value of the exponential operation
	 *
	 * @return this NumberStruct2 raised to the power of the provided NumberStruct2
	 */
	NumberStruct2 expt(NumberStruct2 power);

	/**
	 * Returns the logarithm of this NumberStruct2 with the base value of 'e', the base of the natural logarithms.
	 *
	 * @return the logarithm of this NumberStruct2 with the base value of 'e'
	 */
	NumberStruct2 log();

	/**
	 * Returns the logarithm of this NumberStruct2 with the provided NumberStruct2 base.
	 *
	 * @param base
	 * 		the NumberStruct2 to be used as the base of the logarithmic operation
	 *
	 * @return the logarithm of this NumberStruct2 with the provided NumberStruct2 base
	 */
	NumberStruct2 log(NumberStruct2 base);

	/**
	 * Returns the square-root of this NumberStruct2.
	 *
	 * @return the square-root of this NumberStruct2
	 */
	NumberStruct2 sqrt();

	/**
	 * Returns the 'sine' of this NumberStruct2.
	 *
	 * @return the 'sine' of this NumberStruct2
	 */
	NumberStruct2 sin();

	/**
	 * Returns the 'cosine' of this NumberStruct2.
	 *
	 * @return the 'cosine' of this NumberStruct2
	 */
	NumberStruct2 cos();

	/**
	 * Returns the 'tangent' of this NumberStruct2.
	 *
	 * @return the 'tangent' of this NumberStruct2
	 */
	NumberStruct2 tan();

	/**
	 * Returns the 'Inverse sine' of this NumberStruct2.
	 *
	 * @return the 'Inverse sine' of this NumberStruct2
	 */
	NumberStruct2 asin();

	/**
	 * Returns the 'Inverse cosine' of this NumberStruct2.
	 *
	 * @return the 'Inverse cosine' of this NumberStruct2
	 */
	NumberStruct2 acos();

	/**
	 * Returns the 'Inverse tangent' of this NumberStruct2.
	 *
	 * @return the 'Inverse tangent' of this NumberStruct2
	 */
	NumberStruct2 atan();

	/**
	 * Returns the 'Hyperbolic sine' of this NumberStruct2.
	 *
	 * @return the 'Hyperbolic sine' of this NumberStruct2
	 */
	NumberStruct2 sinh();

	/**
	 * Returns the 'Hyperbolic cosine' of this NumberStruct2.
	 *
	 * @return the 'Hyperbolic cosine' of this NumberStruct2
	 */
	NumberStruct2 cosh();

	/**
	 * Returns the 'Hyperbolic tangent' of this NumberStruct2.
	 *
	 * @return the 'Hyperbolic tangent' of this NumberStruct2
	 */
	NumberStruct2 tanh();

	/**
	 * Returns the 'Inverse hyperbolic sine' of this NumberStruct2.
	 *
	 * @return the 'Inverse hyperbolic sine' of this NumberStruct2
	 */
	NumberStruct2 asinh();

	/**
	 * Returns the 'Inverse hyperbolic cosine' of this NumberStruct2.
	 *
	 * @return the 'Inverse hyperbolic cosine' of this NumberStruct2
	 */
	NumberStruct2 acosh();

	/**
	 * Returns the 'Inverse hyperbolic tangent' of this NumberStruct2.
	 *
	 * @return the 'Inverse hyperbolic tangent' of this NumberStruct2
	 */
	NumberStruct2 atanh();

	/*
		LispStruct
	 */

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the object equality of this NumberStruct with the provided {@link LispStruct}. 'EQL' rules state
	 * that if 'x' and 'y' are both numbers of the same type and the same value, then they are equal.
	 */
	@Override
	default boolean eql(final LispStruct object) {
		return equals(object);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the object equality of this NumberStruct with the provided {@link LispStruct}. 'EQUAL' rules state
	 * that if 'x' and 'y' are 'EQL', then they are equal.
	 */
	@Override
	default boolean equal(final LispStruct object) {
		return eql(object);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the object equality of this NumberStruct with the provided {@link LispStruct}. 'EQUALP' rules state
	 * that if 'x' and 'y' are 'EQL', then they are equal.
	 */
	@Override
	default boolean equalp(final LispStruct object) {
		return (object instanceof NumberStruct2) && isEqualTo((NumberStruct2) object);
	}
}

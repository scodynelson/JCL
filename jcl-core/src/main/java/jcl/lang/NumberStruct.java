/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.util.List;

import jcl.lang.internal.number.ComplexStructImpl;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;

/**
 * The {@link NumberStruct} is the object representation of a Lisp 'number' type.
 */
public interface NumberStruct extends LispStruct {

	/**
	 * Returns a NumberStruct from the provided {@link Apcomplex} value. If the {@link Apcomplex} is an {@link
	 * Apfloat}, {@link RealStruct#valueOf(Apfloat)} is invoked to create the appropriate {@link RealStruct} instead.
	 *
	 * @param apcomplex
	 * 		the {@link Apcomplex} to be used as the value of the resulting NumberStruct
	 *
	 * @return a NumberStruct with the provided {@link Apcomplex} as its value
	 */
	static NumberStruct valueOf(final Apcomplex apcomplex) {
		if (apcomplex instanceof Apfloat) {
			return RealStruct.valueOf((Apfloat) apcomplex);
		}
		// TODO
		return ComplexStructImpl.valueOf(apcomplex, ComplexStruct.ValueType.RATIONAL);
	}

	/**
	 * Returns the {@link Apcomplex} that is used for NumberStruct calculations.
	 *
	 * @return the {@link Apcomplex} that is used for NumberStruct calculations
	 */
	Apcomplex ap();

	/**
	 * Returns the absolute value of this NumberStruct.
	 *
	 * @return the absolute value of this NumberStruct
	 */
	RealStruct abs();

	/**
	 * Returns {@code true} if this NumberStruct is zero; false otherwise.
	 *
	 * @return {@code true} if this NumberStruct is zero; false otherwise
	 */
	boolean zerop();

	/**
	 * Performs the addition operation on this NumberStruct to the provided NumberStruct.
	 *
	 * @param number
	 * 		the NumberStruct to use in the addition operation
	 *
	 * @return the result of the addition operation on this NumberStruct to the provided NumberStruct
	 */
	NumberStruct add(NumberStruct number);

	/**
	 * Performs the addition operation on the {@link List} of NumberStruct objects in order. {@link
	 * IntegerStruct#ZERO} is returned if the list is empty.
	 *
	 * @param numbers
	 * 		the {@link List} of NumberStruct objects to performs the addition operation on
	 *
	 * @return the result of the addition operation against the {@link List} of NumberStruct objects
	 */
	static NumberStruct add(final List<NumberStruct> numbers) {
		return numbers.stream().reduce(IntegerStruct.ZERO, NumberStruct::add);
	}

	/**
	 * Performs the subtraction operation on the provided NumberStruct from this NumberStruct.
	 *
	 * @param number
	 * 		the NumberStruct to use in the subtraction operation
	 *
	 * @return the result of the subtraction operation on the provided NumberStruct from this NumberStruct
	 */
	NumberStruct subtract(NumberStruct number);

	/**
	 * Performs the subtraction operation on the {@link List} of NumberStruct objects in order, using the single
	 * provided NumberStruct as the starting point. The {@link #negation()} of the single provided NumberStruct is
	 * returned if the list is empty.
	 *
	 * @param number
	 * 		the single NumberStruct to use as the starting point of the subtraction operation, or as the base of the
	 * 		{@link #negation()} result if the {@link List} of NumberStruct objects is empty
	 * @param numbers
	 * 		the {@link List} of NumberStruct objects to performs the subtraction operation on
	 *
	 * @return the result of the subtraction operation against the {@link List} of NumberStruct objects
	 */
	static NumberStruct subtract(final NumberStruct number, final List<NumberStruct> numbers) {
		if (numbers.isEmpty()) {
			return number.negation();
		}
		return numbers.stream().reduce(number, NumberStruct::subtract);
	}

	/**
	 * Performs the multiplication operation on this NumberStruct to the provided NumberStruct.
	 *
	 * @param number
	 * 		the NumberStruct to use in the multiplication operation
	 *
	 * @return the result of the multiplication operation on this NumberStruct to the provided NumberStruct
	 */
	NumberStruct multiply(NumberStruct number);

	/**
	 * Performs the multiplication operation on the {@link List} of NumberStruct objects in order. {@link
	 * IntegerStruct#ONE} is returned if the list is empty.
	 *
	 * @param numbers
	 * 		the {@link List} of NumberStruct objects to performs the multiplication operation on
	 *
	 * @return the result of the multiplication operation against the {@link List} of NumberStruct objects
	 */
	static NumberStruct multiply(final List<NumberStruct> numbers) {
		return numbers.stream().reduce(IntegerStruct.ONE, NumberStruct::multiply);
	}

	/**
	 * Performs the division operation on the provided NumberStruct from this NumberStruct.
	 *
	 * @param number
	 * 		the NumberStruct to use in the division operation
	 *
	 * @return the result of the division operation on the provided NumberStruct from this NumberStruct
	 */
	NumberStruct divide(NumberStruct number);

	/**
	 * Performs the division operation on the {@link List} of NumberStruct objects in order, using the single
	 * provided NumberStruct as the starting point. The {@link #reciprocal()} of the single provided NumberStruct is
	 * returned if the list is empty.
	 *
	 * @param number
	 * 		the single NumberStruct to use as the starting point of the division operation, or as the base of the
	 * 		{@link #reciprocal()} result if the {@link List} of NumberStruct objects is empty
	 * @param numbers
	 * 		the {@link List} of NumberStruct objects to performs the division operation on
	 *
	 * @return the result of the division operation against the {@link List} of NumberStruct objects
	 */
	static NumberStruct divide(final NumberStruct number, final List<NumberStruct> numbers) {
		if (numbers.isEmpty()) {
			return number.reciprocal();
		}
		return numbers.stream().reduce(number, NumberStruct::divide);
	}

	/**
	 * Performs a {@literal '=='} comparison of this NumberStruct and the provided NumberStruct.
	 *
	 * @param number
	 * 		the NumberStruct to be used in the {@literal '=='} operation
	 *
	 * @return the {@literal '=='} comparison of this NumberStruct and the provided NumberStruct
	 */
	boolean isEqualTo(NumberStruct number);

	/**
	 * Performs a {@literal '=='} comparison of the provided NumberStruct objects in order, using the single
	 * NumberStruct as the starting point in the comparison. If at any point a value does not follow the expected
	 * comparison, the comparison loop with short-circuit.
	 *
	 * @param number
	 * 		the initial NumberStruct used in the {@literal '=='} comparison
	 * @param numbers
	 * 		the {@link List} of NumberStruct objects used in the {@literal '=='} comparison
	 *
	 * @return the {@literal '=='} comparison provided NumberStruct objects
	 */
	static boolean isEqualTo(final NumberStruct number, final List<NumberStruct> numbers) {
		NumberStruct previousNumber = number;

		boolean result = true;
		for (final NumberStruct currentNumber : numbers) {
			result = previousNumber.isEqualTo(currentNumber);
			if (!result) {
				break;
			}
			previousNumber = currentNumber;
		}
		return result;
	}

	/**
	 * Performs a {@literal '!='} comparison of this NumberStruct and the provided NumberStruct.
	 *
	 * @param number
	 * 		the NumberStruct to be used in the {@literal '!='} operation
	 *
	 * @return the {@literal '!='} comparison of this NumberStruct and the provided NumberStruct
	 */
	default boolean isNotEqualTo(final NumberStruct number) {
		return !isEqualTo(number);
	}

	/**
	 * Performs a {@literal '!='} comparison of the provided NumberStruct objects in order, using the single
	 * NumberStruct as the starting point in the comparison. If at any point a value does not follow the expected
	 * comparison, the comparison loop with short-circuit.
	 *
	 * @param number
	 * 		the initial NumberStruct used in the {@literal '!='} comparison
	 * @param numbers
	 * 		the {@link List} of NumberStruct objects used in the {@literal '!='} comparison
	 *
	 * @return the {@literal '!='} comparison provided NumberStruct objects
	 */
	static boolean isNotEqualTo(final NumberStruct number, final List<NumberStruct> numbers) {
		NumberStruct previousNumber = number;

		boolean result = true;
		for (final NumberStruct currentNumber : numbers) {
			result = previousNumber.isNotEqualTo(currentNumber);
			if (!result) {
				break;
			}
			previousNumber = currentNumber;
		}
		return result;
	}

	/**
	 * Determines the numerical values that indicates whether this NumberStruct is negative, zero, or positive.
	 *
	 * @return the numerical values that indicates whether this NumberStruct is negative, zero, or positive
	 */
	NumberStruct signum();

	/**
	 * Returns the 'real' part of this NumberStruct.
	 *
	 * @return the 'real' part of this NumberStruct
	 */
	RealStruct realPart();

	/**
	 * Returns the 'imaginary' part of this NumberStruct.
	 *
	 * @return the 'imaginary' part of this NumberStruct
	 */
	RealStruct imagPart();

	/**
	 * Returns the complex conjugate of this NumberStruct.
	 *
	 * @return the complex conjugate of this NumberStruct
	 */
	NumberStruct conjugate();

	/**
	 * Returns the negation of this NumberStruct.
	 *
	 * @return the negation of this NumberStruct
	 */
	NumberStruct negation();

	/**
	 * Returns the reciprocal of this NumberStruct.
	 *
	 * @return the reciprocal of this NumberStruct
	 */
	NumberStruct reciprocal();

	/**
	 * Returns 'e' raised to the power of this NumberStruct, where 'e' is the base of the natural logarithms.
	 *
	 * @return 'e' raised to the power of this NumberStruct
	 */
	NumberStruct exp();

	/**
	 * Returns the result of this NumberStruct raised to the power of the provided NumberStruct.
	 *
	 * @param power
	 * 		the NumberStruct to use as the power value of the exponential operation
	 *
	 * @return this NumberStruct raised to the power of the provided NumberStruct
	 */
	NumberStruct expt(NumberStruct power);

	/**
	 * Returns the logarithm of this NumberStruct with the base value of 'e', the base of the natural logarithms.
	 *
	 * @return the logarithm of this NumberStruct with the base value of 'e'
	 */
	NumberStruct log();

	/**
	 * Returns the logarithm of this NumberStruct with the provided NumberStruct base.
	 *
	 * @param base
	 * 		the NumberStruct to be used as the base of the logarithmic operation
	 *
	 * @return the logarithm of this NumberStruct with the provided NumberStruct base
	 */
	NumberStruct log(NumberStruct base);

	/**
	 * Returns the square-root of this NumberStruct.
	 *
	 * @return the square-root of this NumberStruct
	 */
	NumberStruct sqrt();

	/**
	 * Returns the 'sine' of this NumberStruct.
	 *
	 * @return the 'sine' of this NumberStruct
	 */
	NumberStruct sin();

	/**
	 * Returns the 'cosine' of this NumberStruct.
	 *
	 * @return the 'cosine' of this NumberStruct
	 */
	NumberStruct cos();

	/**
	 * Returns the 'tangent' of this NumberStruct.
	 *
	 * @return the 'tangent' of this NumberStruct
	 */
	NumberStruct tan();

	/**
	 * Returns the 'Inverse sine' of this NumberStruct.
	 *
	 * @return the 'Inverse sine' of this NumberStruct
	 */
	NumberStruct asin();

	/**
	 * Returns the 'Inverse cosine' of this NumberStruct.
	 *
	 * @return the 'Inverse cosine' of this NumberStruct
	 */
	NumberStruct acos();

	/**
	 * Returns the 'Inverse tangent' of this NumberStruct.
	 *
	 * @return the 'Inverse tangent' of this NumberStruct
	 */
	NumberStruct atan();

	/**
	 * Returns the 'Hyperbolic sine' of this NumberStruct.
	 *
	 * @return the 'Hyperbolic sine' of this NumberStruct
	 */
	NumberStruct sinh();

	/**
	 * Returns the 'Hyperbolic cosine' of this NumberStruct.
	 *
	 * @return the 'Hyperbolic cosine' of this NumberStruct
	 */
	NumberStruct cosh();

	/**
	 * Returns the 'Hyperbolic tangent' of this NumberStruct.
	 *
	 * @return the 'Hyperbolic tangent' of this NumberStruct
	 */
	NumberStruct tanh();

	/**
	 * Returns the 'Inverse hyperbolic sine' of this NumberStruct.
	 *
	 * @return the 'Inverse hyperbolic sine' of this NumberStruct
	 */
	NumberStruct asinh();

	/**
	 * Returns the 'Inverse hyperbolic cosine' of this NumberStruct.
	 *
	 * @return the 'Inverse hyperbolic cosine' of this NumberStruct
	 */
	NumberStruct acosh();

	/**
	 * Returns the 'Inverse hyperbolic tangent' of this NumberStruct.
	 *
	 * @return the 'Inverse hyperbolic tangent' of this NumberStruct
	 */
	NumberStruct atanh();

	/*
		LispStruct
	 */

	@Override
	default boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof NumberStruct)
						&& ((NumberStruct) object).ap().equals(ap()));
	}

	@Override
	default boolean equalp(final LispStruct object) {
		return equal(object) ||
				((object instanceof NumberStruct)
						&& isEqualTo((NumberStruct) object));
	}
}

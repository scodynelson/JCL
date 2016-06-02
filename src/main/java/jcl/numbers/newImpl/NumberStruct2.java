/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import java.util.List;

import jcl.LispStruct;
import org.apache.commons.math3.fraction.BigFraction;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;

/**
 * The {@link NumberStruct2} is the object representation of a Lisp 'number' type.
 */
public interface NumberStruct2 extends LispStruct {

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

	Apcomplex ap();

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the absolute value of this RatioStruct.
	 */
	RealStruct2 abs();

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this RatioStruct is zero by comparing {@code #bigFraction} to {@link
	 * BigFraction#ZERO}.
	 */
	boolean zerop();

	NumberStruct2 add(NumberStruct2 number);

	static NumberStruct2 add(final List<NumberStruct2> numbers) {
		return numbers.stream().reduce(IntegerStruct2.ZERO, NumberStruct2::add);
	}

	NumberStruct2 subtract(NumberStruct2 number);

	static NumberStruct2 subtract(final NumberStruct2 number, final List<NumberStruct2> numbers) {
		if (numbers.isEmpty()) {
			return number.negation();
		}
		return numbers.stream().reduce(number, NumberStruct2::subtract);
	}

	NumberStruct2 multiply(NumberStruct2 number);

	static NumberStruct2 multiply(final List<NumberStruct2> numbers) {
		return numbers.stream().reduce(IntegerStruct2.ONE, NumberStruct2::multiply);
	}

	NumberStruct2 divide(NumberStruct2 number);

	static NumberStruct2 divide(final NumberStruct2 number, final List<NumberStruct2> numbers) {
		if (numbers.isEmpty()) {
			return number.reciprocal();
		}
		return numbers.stream().reduce(number, NumberStruct2::divide);
	}

	boolean isEqualTo(NumberStruct2 number);

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

	default boolean isNotEqualTo(final NumberStruct2 number) {
		return !isEqualTo(number);
	}

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
	 * {@inheritDoc}
	 * <p>
	 * Determines the whether or not the numerical value of this RationalStruct is zero, positive, or negative,
	 * returning {@code this}, {@link IntegerStruct2#ONE}, or {@link IntegerStruct2#MINUS_ONE} respectively.
	 */
	NumberStruct2 signum();

	RealStruct2 realPart();

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link IntegerStruct2#ONE} as the imaginary part of RationalStructs is always '1'.
	 */
	RealStruct2 imagPart();

	NumberStruct2 conjugate();

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the negation with {@link BigFraction#negate()} on {@code #bigFraction} and then creating a new
	 * RatioStruct to wrap it.
	 */
	NumberStruct2 negation();

	/**
	 * {@inheritDoc}
	 * <p>
	 * Creates a new {@link RationalStruct2} with {@link BigFraction#denominator} as the numerator and {@link
	 * BigFraction#numerator} as the denominator from {@code #bigFraction}.
	 */
	NumberStruct2 reciprocal();

	NumberStruct2 exp();

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the exponential function result for this RatioStruct as this {@code base} and the provided {@link
	 * NumberStruct2} as the {@code power}. If {@code power} is '0' and power is an {@link IntegerStruct2}, {@link
	 * IntegerStruct2#ONE} is returned. If {@code power} is '0' and power is not an {@link IntegerStruct2}, {@link
	 * FloatStruct2#ONE} is returned. If this RatioStruct is either '0' or '1', {@code this} is returned.
	 */
	NumberStruct2 expt(NumberStruct2 power);

	NumberStruct2 log();

	NumberStruct2 log(NumberStruct2 base);

	NumberStruct2 sqrt();

	NumberStruct2 sin();

	NumberStruct2 cos();

	NumberStruct2 tan();

	NumberStruct2 asin();

	NumberStruct2 acos();

	NumberStruct2 atan();

	NumberStruct2 sinh();

	NumberStruct2 cosh();

	NumberStruct2 tanh();

	NumberStruct2 asinh();

	NumberStruct2 acosh();

	NumberStruct2 atanh();

	static NumberStruct2 valueOf(final Apcomplex apcomplex) {
		if (apcomplex instanceof Apfloat) {
			return RealStruct2.valueOf((Apfloat) apcomplex);
		}
		return ComplexStruct2.valueOf(apcomplex);
	}
}

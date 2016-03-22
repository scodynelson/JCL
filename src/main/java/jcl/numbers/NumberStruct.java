/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.util.List;

import jcl.LispStruct;
import org.apfloat.Apcomplex;

/**
 * The {@link NumberStruct} is the object representation of a Lisp 'number' type.
 */
public interface NumberStruct extends LispStruct {

	// TODO: move the following 3 up

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the object equality of this NumberStruct with the provided {@link LispStruct}. 'EQL' rules state
	 * that if 'x' and 'y' are both numbers of the same type and the same value, then they are equal.
	 */
//	@Override
	default boolean lispEql(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the object equality of this NumberStruct with the provided {@link LispStruct}. 'EQUAL' rules state
	 * that if 'x' and 'y' are 'EQL', then they are equal.
	 */
//	@Override
	default boolean lispEqual(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the object equality of this NumberStruct with the provided {@link LispStruct}. 'EQUALP' rules state
	 * that if 'x' and 'y' are 'EQL', then they are equal.
	 */
//	@Override
	default boolean lispEqualp(final LispStruct lispStruct) {
		return (lispStruct instanceof NumberStruct) && isEqualTo((NumberStruct) lispStruct);
	}

	Apcomplex apcomplexValue();

	RealStruct abs();

	boolean zerop();

	default NumberStruct add(final NumberStruct number) {
		final AddVisitor<?> addVisitor = addVisitor();
		return number.add(addVisitor);
	}

	NumberStruct add(AddVisitor<?> addVisitor);

	AddVisitor<?> addVisitor();

	static NumberStruct add(final List<NumberStruct> numbers) {
		return numbers.stream().reduce(IntegerStruct.ZERO, NumberStruct::add);
	}

	default NumberStruct subtract(final NumberStruct number) {
		final SubtractVisitor<?> subtractVisitor = subtractVisitor();
		return number.subtract(subtractVisitor);
	}

	NumberStruct subtract(SubtractVisitor<?> subtractVisitor);

	SubtractVisitor<?> subtractVisitor();

	static NumberStruct subtract(final NumberStruct number, final List<NumberStruct> numbers) {
		if (numbers.isEmpty()) {
			return number.negation();
		}
		return numbers.stream().reduce(number, NumberStruct::subtract);
	}

	default NumberStruct multiply(final NumberStruct number) {
		final MultiplyVisitor<?> multiplyVisitor = multiplyVisitor();
		return number.multiply(multiplyVisitor);
	}

	NumberStruct multiply(MultiplyVisitor<?> multiplyVisitor);

	MultiplyVisitor<?> multiplyVisitor();

	static NumberStruct multiply(final List<NumberStruct> numbers) {
		return numbers.stream().reduce(IntegerStruct.ONE, NumberStruct::multiply);
	}

	default NumberStruct divide(final NumberStruct number) {
		final DivideVisitor<?> divideVisitor = divideVisitor();
		return number.divide(divideVisitor);
	}

	NumberStruct divide(DivideVisitor<?> divideVisitor);

	DivideVisitor<?> divideVisitor();

	static NumberStruct divide(final NumberStruct number, final List<NumberStruct> numbers) {
		if (numbers.isEmpty()) {
			return number.reciprocal();
		}
		return numbers.stream().reduce(number, NumberStruct::divide);
	}

	default boolean isEqualTo(final NumberStruct number) {
		final EqualToVisitor<?> equalToVisitor = equalToVisitor();
		return number.isEqualTo(equalToVisitor);
	}

	boolean isEqualTo(EqualToVisitor<?> equalToVisitor);

	EqualToVisitor<?> equalToVisitor();

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

	default boolean isNotEqualTo(final NumberStruct number) {
		return !isEqualTo(number);
	}

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

	NumberStruct signum();

	NumberStruct realPart();

	NumberStruct imagPart();

	NumberStruct conjugate();

	NumberStruct negation();

	NumberStruct reciprocal();

	NumberStruct exp();

	default NumberStruct expt(final NumberStruct power) {
//		final Apcomplex baseApcomplex = apcomplexValue();
//		final Apcomplex powerApcomplex = power.apcomplexValue();
//		final Apcomplex pow = ApcomplexMath.pow(baseApcomplex, powerApcomplex);
//
//		if (pow instanceof Apint) {
//			return new IntegerStruct((Apint) pow);
//		} else if (pow instanceof Apfloat) {
//			return new FloatStruct((Apfloat) pow);
//		} else {
//			return new ComplexStruct(pow);
//		}
		final ExptVisitor<?> exptVisitor = exptVisitor();
		return power.expt(exptVisitor);
	}

	NumberStruct expt(ExptVisitor<?> exptVisitor);

	ExptVisitor<?> exptVisitor();

	NumberStruct log();

	default NumberStruct log(final NumberStruct base) {
		return log().divide(base.log());
	}

	NumberStruct sqrt();

	NumberStruct sin();

	NumberStruct cos();

	NumberStruct tan();

	NumberStruct asin();

	NumberStruct acos();

	NumberStruct atan();

	NumberStruct sinh();

	NumberStruct cosh();

	NumberStruct tanh();

	NumberStruct asinh();

	NumberStruct acosh();

	NumberStruct atanh();

	// Visitor Implementations

	abstract class AddVisitor<S extends NumberStruct> {

		final S number1;

		AddVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct add(IntegerStruct number2);

		public abstract NumberStruct add(FloatStruct number2);

		public abstract NumberStruct add(RatioStruct number2);

		public abstract NumberStruct add(ComplexStruct number2);
	}

	abstract class SubtractVisitor<S extends NumberStruct> {

		final S number1;

		SubtractVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct subtract(IntegerStruct number2);

		public abstract NumberStruct subtract(FloatStruct number2);

		public abstract NumberStruct subtract(RatioStruct number2);

		public abstract NumberStruct subtract(ComplexStruct number2);
	}

	abstract class MultiplyVisitor<S extends NumberStruct> {

		final S number1;

		MultiplyVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct multiply(IntegerStruct number2);

		public abstract NumberStruct multiply(FloatStruct number2);

		public abstract NumberStruct multiply(RatioStruct number2);

		public abstract NumberStruct multiply(ComplexStruct number2);
	}

	abstract class DivideVisitor<S extends NumberStruct> {

		final S number1;

		DivideVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct divide(IntegerStruct number2);

		public abstract NumberStruct divide(FloatStruct number2);

		public abstract NumberStruct divide(RatioStruct number2);

		public abstract NumberStruct divide(ComplexStruct number2);
	}

	abstract class EqualToVisitor<S extends NumberStruct> {

		final S number1;

		EqualToVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract boolean equalTo(IntegerStruct number2);

		public abstract boolean equalTo(FloatStruct number2);

		public abstract boolean equalTo(RatioStruct number2);

		public abstract boolean equalTo(ComplexStruct number2);
	}

	abstract class ExptVisitor<S extends NumberStruct> {

		final S base;

		ExptVisitor(final S base) {
			this.base = base;
		}

		public abstract NumberStruct expt(IntegerStruct power);

		public abstract NumberStruct expt(FloatStruct power);

		public abstract NumberStruct expt(RatioStruct power);

		public abstract NumberStruct expt(ComplexStruct power);

		static NumberStruct exptInteger(final NumberStruct base, final IntegerStruct power) {
			// TODO: simplify this!!!
			if (power.isEqualTo(IntegerStruct.ZERO)) {
				return IntegerStruct.ONE;
			}
			if (base.isEqualTo(IntegerStruct.ONE)) {
				return base;
			}
			if (base.isEqualTo(IntegerStruct.ZERO)) {
				return base;
			}

			IntegerStruct realPower = power;
			if (realPower.minusp()) {
				realPower = (IntegerStruct) IntegerStruct.ZERO.subtract(realPower);
				return IntegerStruct.ONE.divide(exptInteger(base, realPower));
			}
			if (base.lispEql(IntegerStruct.TWO)) {
				return IntegerStruct.ONE.ash(realPower);
			}

			IntegerStruct nextn = realPower.ash(IntegerStruct.MINUS_ONE);
			NumberStruct total;
			if (realPower.oddp()) {
				total = base;
			} else {
				total = IntegerStruct.ONE;
			}

			NumberStruct realBase = base;
			while (true) {
				if (nextn.zerop()) {
					return total;
				}
				realBase = realBase.multiply(realBase);

				if (nextn.oddp()) {
					total = realBase.multiply(total);
				}
				nextn = nextn.ash(IntegerStruct.MINUS_ONE);
			}
		}
	}
}

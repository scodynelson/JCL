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
		return (object instanceof NumberStruct) && isEqualTo((NumberStruct) object);
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
//			return new IntIntegerStruct((Apint) pow);
//		} else if (pow instanceof Apfloat) {
//			return new SingleFloatStruct((Apfloat) pow);
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

		public abstract NumberStruct add(IntIntegerStruct number2);

		public abstract NumberStruct add(LongIntegerStruct number2);

		public abstract NumberStruct add(BigIntegerStruct number2);

		public abstract NumberStruct add(SingleFloatStruct number2);

		public abstract NumberStruct add(DoubleFloatStruct number2);

		public abstract NumberStruct add(BigFloatStruct number2);

		public abstract NumberStruct add(RatioStruct number2);

		public abstract NumberStruct add(ComplexStruct number2);
	}

	abstract class SubtractVisitor<S extends NumberStruct> {

		final S number1;

		SubtractVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct subtract(IntIntegerStruct number2);

		public abstract NumberStruct subtract(LongIntegerStruct number2);

		public abstract NumberStruct subtract(BigIntegerStruct number2);

		public abstract NumberStruct subtract(SingleFloatStruct number2);

		public abstract NumberStruct subtract(DoubleFloatStruct number2);

		public abstract NumberStruct subtract(BigFloatStruct number2);

		public abstract NumberStruct subtract(RatioStruct number2);

		public abstract NumberStruct subtract(ComplexStruct number2);
	}

	abstract class MultiplyVisitor<S extends NumberStruct> {

		final S number1;

		MultiplyVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct multiply(IntIntegerStruct number2);

		public abstract NumberStruct multiply(LongIntegerStruct number2);

		public abstract NumberStruct multiply(BigIntegerStruct number2);

		public abstract NumberStruct multiply(SingleFloatStruct number2);

		public abstract NumberStruct multiply(DoubleFloatStruct number2);

		public abstract NumberStruct multiply(BigFloatStruct number2);

		public abstract NumberStruct multiply(RatioStruct number2);

		public abstract NumberStruct multiply(ComplexStruct number2);
	}

	abstract class DivideVisitor<S extends NumberStruct> {

		final S number1;

		DivideVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct divide(IntIntegerStruct number2);

		public abstract NumberStruct divide(LongIntegerStruct number2);

		public abstract NumberStruct divide(BigIntegerStruct number2);

		public abstract NumberStruct divide(SingleFloatStruct number2);

		public abstract NumberStruct divide(DoubleFloatStruct number2);

		public abstract NumberStruct divide(BigFloatStruct number2);

		public abstract NumberStruct divide(RatioStruct number2);

		public abstract NumberStruct divide(ComplexStruct number2);
	}

	abstract class EqualToVisitor<S extends NumberStruct> {

		final S number1;

		EqualToVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract boolean equalTo(IntIntegerStruct number2);

		public abstract boolean equalTo(LongIntegerStruct number2);

		public abstract boolean equalTo(BigIntegerStruct number2);

		public abstract boolean equalTo(SingleFloatStruct number2);

		public abstract boolean equalTo(DoubleFloatStruct number2);

		public abstract boolean equalTo(BigFloatStruct number2);

		public abstract boolean equalTo(RatioStruct number2);

		public abstract boolean equalTo(ComplexStruct number2);
	}

	abstract class ExptVisitor<S extends NumberStruct> {

		final S base;

		ExptVisitor(final S base) {
			this.base = base;
		}

		public abstract NumberStruct expt(IntIntegerStruct power);

		public abstract NumberStruct expt(LongIntegerStruct power);

		public abstract NumberStruct expt(BigIntegerStruct power);

		public abstract NumberStruct expt(SingleFloatStruct power);

		public abstract NumberStruct expt(DoubleFloatStruct power);

		public abstract NumberStruct expt(BigFloatStruct power);

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
			if (base.eql(IntegerStruct.TWO)) {
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

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.util.List;

import jcl.LispStruct;
import jcl.classes.BuiltInClassStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.types.NumberType;

/**
 * The {@link NumberStruct} is the object representation of a Lisp 'number' type.
 */
public abstract class NumberStruct extends BuiltInClassStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -506448097593323391L;

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected NumberStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(NumberType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the number object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected NumberStruct(final NumberType type,
	                       final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	// TODO: move the following 3 up
	public abstract boolean eql(LispStruct lispStruct);

	public abstract boolean equal(LispStruct lispStruct);

	public abstract boolean equalp(LispStruct lispStruct);

	public abstract RealStruct abs();

	public abstract boolean zerop();

	public abstract NumberStruct add(final NumberStruct number);

	public abstract NumberStruct subtract(final NumberStruct number);

	public abstract NumberStruct multiply(final NumberStruct number);

	public abstract NumberStruct divide(final NumberStruct number);

	public abstract boolean isEqualTo(final NumberStruct number);

	public boolean isNotEqualTo(final NumberStruct number) {
		return !isEqualTo(number);
	}

	public abstract NumberStruct signum();

	public abstract NumberStruct realPart();

	public abstract NumberStruct imagPart();

	public abstract NumberStruct conjugate();

	public abstract NumberStruct negation();

	public abstract NumberStruct reciprocal();

	public abstract NumberStruct exp();

	public abstract NumberStruct expt(NumberStruct power);

	public abstract NumberStruct log();

	public NumberStruct log(final NumberStruct base) {
		return log().divide(base.log());
	}

	public abstract NumberStruct sqrt();

	public abstract NumberStruct sin();

	public abstract NumberStruct cos();

	public abstract NumberStruct tan();

	public abstract NumberStruct asin();

	public abstract NumberStruct acos();

	public abstract NumberStruct atan();

	public abstract NumberStruct sinh();

	public abstract NumberStruct cosh();

	public abstract NumberStruct tanh();

	public abstract NumberStruct asinh();

	public abstract NumberStruct acosh();

	public abstract NumberStruct atanh();

	// Strategy Implementations

	protected abstract static class AddStrategy<S extends NumberStruct> {

		public NumberStruct add(final S number1, final NumberStruct number2) {
			if (number2 instanceof IntegerStruct) {
				return add(number1, (IntegerStruct) number2);
			} else if (number2 instanceof FloatStruct) {
				return add(number1, (FloatStruct) number2);
			} else if (number2 instanceof RatioStruct) {
				return add(number1, (RatioStruct) number2);
			} else if (number2 instanceof ComplexStruct) {
				return add(number1, (ComplexStruct) number2);
			} else {
				throw new RuntimeException("Unsupported Number Type for Add Operation.");
			}
		}

		public abstract NumberStruct add(S number1, IntegerStruct number2);

		public abstract NumberStruct add(S number1, FloatStruct number2);

		public abstract NumberStruct add(S number1, RatioStruct number2);

		public abstract NumberStruct add(S number1, ComplexStruct number2);
	}

	protected abstract static class SubtractStrategy<S extends NumberStruct> {

		public NumberStruct subtract(final S number1, final NumberStruct number2) {
			if (number2 instanceof IntegerStruct) {
				return subtract(number1, (IntegerStruct) number2);
			} else if (number2 instanceof FloatStruct) {
				return subtract(number1, (FloatStruct) number2);
			} else if (number2 instanceof RatioStruct) {
				return subtract(number1, (RatioStruct) number2);
			} else if (number2 instanceof ComplexStruct) {
				return subtract(number1, (ComplexStruct) number2);
			} else {
				throw new RuntimeException("Unsupported Number Type for Subtract Operation.");
			}
		}

		public abstract NumberStruct subtract(S number1, IntegerStruct number2);

		public abstract NumberStruct subtract(S number1, FloatStruct number2);

		public abstract NumberStruct subtract(S number1, RatioStruct number2);

		public abstract NumberStruct subtract(S number1, ComplexStruct number2);
	}

	protected abstract static class MultiplyStrategy<S extends NumberStruct> {

		public NumberStruct multiply(final S number1, final NumberStruct number2) {
			if (number2 instanceof IntegerStruct) {
				return multiply(number1, (IntegerStruct) number2);
			} else if (number2 instanceof FloatStruct) {
				return multiply(number1, (FloatStruct) number2);
			} else if (number2 instanceof RatioStruct) {
				return multiply(number1, (RatioStruct) number2);
			} else if (number2 instanceof ComplexStruct) {
				return multiply(number1, (ComplexStruct) number2);
			} else {
				throw new RuntimeException("Unsupported Number Type for Multiply Operation.");
			}
		}

		public abstract NumberStruct multiply(S number1, IntegerStruct number2);

		public abstract NumberStruct multiply(S number1, FloatStruct number2);

		public abstract NumberStruct multiply(S number1, RatioStruct number2);

		public abstract NumberStruct multiply(S number1, ComplexStruct number2);
	}

	protected abstract static class DivideStrategy<S extends NumberStruct> {

		public NumberStruct divide(final S number1, final NumberStruct number2) {
			if (number2 instanceof IntegerStruct) {
				return divide(number1, (IntegerStruct) number2);
			} else if (number2 instanceof FloatStruct) {
				return divide(number1, (FloatStruct) number2);
			} else if (number2 instanceof RatioStruct) {
				return divide(number1, (RatioStruct) number2);
			} else if (number2 instanceof ComplexStruct) {
				return divide(number1, (ComplexStruct) number2);
			} else {
				throw new RuntimeException("Unsupported Number Type for Divide Operation.");
			}
		}

		public abstract NumberStruct divide(S number1, IntegerStruct number2);

		public abstract NumberStruct divide(S number1, FloatStruct number2);

		public abstract NumberStruct divide(S number1, RatioStruct number2);

		public abstract NumberStruct divide(S number1, ComplexStruct number2);
	}

	protected abstract static class EqualToStrategy<S extends NumberStruct> {

		public boolean equalTo(final S number1, final NumberStruct number2) {
			if (number2 instanceof IntegerStruct) {
				return equalTo(number1, (IntegerStruct) number2);
			} else if (number2 instanceof FloatStruct) {
				return equalTo(number1, (FloatStruct) number2);
			} else if (number2 instanceof RatioStruct) {
				return equalTo(number1, (RatioStruct) number2);
			} else if (number2 instanceof ComplexStruct) {
				return equalTo(number1, (ComplexStruct) number2);
			} else {
				throw new RuntimeException("Unsupported Number Type for EqualTo Operation.");
			}
		}

		public abstract boolean equalTo(S number1, IntegerStruct number2);

		public abstract boolean equalTo(S number1, FloatStruct number2);

		public abstract boolean equalTo(S number1, RatioStruct number2);

		public abstract boolean equalTo(S number1, ComplexStruct number2);
	}

	protected abstract static class ExptStrategy<S extends NumberStruct> {

		public NumberStruct expt(final S number1, final NumberStruct number2) {
			if (number2 instanceof IntegerStruct) {
				return expt(number1, (IntegerStruct) number2);
			} else if (number2 instanceof FloatStruct) {
				return expt(number1, (FloatStruct) number2);
			} else if (number2 instanceof RatioStruct) {
				return expt(number1, (RatioStruct) number2);
			} else if (number2 instanceof ComplexStruct) {
				return expt(number1, (ComplexStruct) number2);
			} else {
				throw new RuntimeException("Unsupported Number Type for Expt Operation.");
			}
		}

		public abstract NumberStruct expt(S number1, IntegerStruct number2);

		public abstract NumberStruct expt(S number1, FloatStruct number2);

		public abstract NumberStruct expt(S number1, RatioStruct number2);

		public abstract NumberStruct expt(S number1, ComplexStruct number2);

		protected static NumberStruct exptInteger(final NumberStruct base, final IntegerStruct power) {
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

	// Static Multi-Arg Methods

	public static NumberStruct add(final NumberStruct... numbers) {
		if (numbers.length == 0) {
			return IntegerStruct.ZERO;
		}

		NumberStruct result = numbers[0];
		for (int i = 1; i < numbers.length; i++) {
			final NumberStruct currentNumber = numbers[i];
			result = result.add(currentNumber);
		}
		return result;
	}

	public static NumberStruct subtract(final NumberStruct... numbers) {
		if (numbers.length == 0) {
			throw new ErrorException("At least one number required perform subtraction.");
		}
		if (numbers.length == 1) {
			return numbers[0].negation();
		}

		NumberStruct result = numbers[0];
		for (int i = 1; i < numbers.length; i++) {
			final NumberStruct currentNumber = numbers[i];
			result = result.subtract(currentNumber);
		}
		return result;
	}

	public static NumberStruct multiply(final NumberStruct... numbers) {
		if (numbers.length == 0) {
			return IntegerStruct.ONE;
		}

		NumberStruct result = numbers[0];
		for (int i = 1; i < numbers.length; i++) {
			final NumberStruct currentNumber = numbers[i];
			result = result.multiply(currentNumber);
		}
		return result;
	}

	public static NumberStruct divide(final NumberStruct... numbers) {
		if (numbers.length == 0) {
			throw new ErrorException("At least one number required to perform division.");
		}
		if (numbers.length == 1) {
			return numbers[0].reciprocal();
		}

		NumberStruct result = numbers[0];
		for (int i = 1; i < numbers.length; i++) {
			final NumberStruct currentNumber = numbers[i];
			result = result.divide(currentNumber);
		}
		return result;
	}

	public static boolean isEqualTo(final NumberStruct... numbers) {
		if (numbers.length == 0) {
			throw new ErrorException("At least one number required to test equality.");
		}

		NumberStruct previousNumber = numbers[0];

		boolean result = true;
		for (int i = 1; i < numbers.length; i++) {
			final NumberStruct currentNumber = numbers[i];
			result = previousNumber.isEqualTo(currentNumber);
			if (!result) {
				break;
			}
			previousNumber = currentNumber;
		}
		return result;
	}

	public static boolean isNotEqualTo(final NumberStruct... numbers) {
		if (numbers.length == 0) {
			throw new ErrorException("At least one number required to test equality.");
		}

		NumberStruct previousNumber = numbers[0];

		boolean result = true;
		for (int i = 1; i < numbers.length; i++) {
			final NumberStruct currentNumber = numbers[i];
			result = previousNumber.isNotEqualTo(currentNumber);
			if (!result) {
				break;
			}
			previousNumber = currentNumber;
		}
		return result;
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.util.List;

import jcl.LispStruct;
import jcl.classes.BuiltInClassStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.types.NumberType;
import org.apfloat.Apcomplex;

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
	public abstract boolean lispEql(LispStruct lispStruct);

	public abstract boolean lispEqual(LispStruct lispStruct);

	public abstract boolean lispEqualp(LispStruct lispStruct);

	public abstract Apcomplex apcomplexValue();

	public abstract RealStruct abs();

	public abstract boolean zerop();

	public NumberStruct add(final NumberStruct number) {
		final AddStrategy<?> addStrategy = getAddStrategy();
		return number.add(addStrategy);
	}

	protected abstract NumberStruct add(final AddStrategy<?> addStrategy);

	protected abstract AddStrategy<?> getAddStrategy();

	public NumberStruct subtract(final NumberStruct number) {
		final SubtractStrategy<?> subtractStrategy = getSubtractStrategy();
		return number.subtract(subtractStrategy);
	}

	protected abstract NumberStruct subtract(final SubtractStrategy<?> subtractStrategy);

	protected abstract SubtractStrategy<?> getSubtractStrategy();

	public NumberStruct multiply(final NumberStruct number) {
		final MultiplyStrategy<?> multiplyStrategy = getMultiplyStrategy();
		return number.multiply(multiplyStrategy);
	}

	protected abstract NumberStruct multiply(final MultiplyStrategy<?> multiplyStrategy);

	protected abstract MultiplyStrategy<?> getMultiplyStrategy();

	public NumberStruct divide(final NumberStruct number) {
		final DivideStrategy<?> divideStrategy = getDivideStrategy();
		return number.divide(divideStrategy);
	}

	protected abstract NumberStruct divide(final DivideStrategy<?> divideStrategy);

	protected abstract DivideStrategy<?> getDivideStrategy();

	public boolean isEqualTo(final NumberStruct number) {
		final EqualToStrategy<?> equalToStrategy = getEqualToStrategy();
		return number.isEqualTo(equalToStrategy);
	}

	protected abstract boolean isEqualTo(final EqualToStrategy<?> equalToStrategy);

	protected abstract EqualToStrategy<?> getEqualToStrategy();

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

	public NumberStruct expt(final NumberStruct power) {
		final ExptStrategy<?> exptStrategy = getExptStrategy();
		return power.expt(exptStrategy);
	}

	protected abstract NumberStruct expt(final ExptStrategy<?> exptStrategy);

	protected abstract ExptStrategy<?> getExptStrategy();

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

		protected final S number1;

		protected AddStrategy(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct add(IntegerStruct number2);

		public abstract NumberStruct add(FloatStruct number2);

		public abstract NumberStruct add(RatioStruct number2);

		public abstract NumberStruct add(ComplexStruct number2);
	}

	protected abstract static class SubtractStrategy<S extends NumberStruct> {

		protected final S number1;

		protected SubtractStrategy(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct subtract(IntegerStruct number2);

		public abstract NumberStruct subtract(FloatStruct number2);

		public abstract NumberStruct subtract(RatioStruct number2);

		public abstract NumberStruct subtract(ComplexStruct number2);
	}

	protected abstract static class MultiplyStrategy<S extends NumberStruct> {

		protected final S number1;

		protected MultiplyStrategy(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct multiply(IntegerStruct number2);

		public abstract NumberStruct multiply(FloatStruct number2);

		public abstract NumberStruct multiply(RatioStruct number2);

		public abstract NumberStruct multiply(ComplexStruct number2);
	}

	protected abstract static class DivideStrategy<S extends NumberStruct> {

		protected final S number1;

		protected DivideStrategy(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct divide(IntegerStruct number2);

		public abstract NumberStruct divide(FloatStruct number2);

		public abstract NumberStruct divide(RatioStruct number2);

		public abstract NumberStruct divide(ComplexStruct number2);
	}

	protected abstract static class EqualToStrategy<S extends NumberStruct> {

		protected final S number1;

		protected EqualToStrategy(final S number1) {
			this.number1 = number1;
		}

		public abstract boolean equalTo(IntegerStruct number2);

		public abstract boolean equalTo(FloatStruct number2);

		public abstract boolean equalTo(RatioStruct number2);

		public abstract boolean equalTo(ComplexStruct number2);
	}

	protected abstract static class ExptStrategy<S extends NumberStruct> {

		protected final S base;

		protected ExptStrategy(final S base) {
			this.base = base;
		}

		public abstract NumberStruct expt(IntegerStruct power);

		public abstract NumberStruct expt(FloatStruct power);

		public abstract NumberStruct expt(RatioStruct power);

		public abstract NumberStruct expt(ComplexStruct power);

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

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
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.apfloat.Apint;

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
		final AddVisitor<?> addVisitor = addVisitor();
		return number.add(addVisitor);
	}

	protected abstract NumberStruct add(final AddVisitor<?> addVisitor);

	protected abstract AddVisitor<?> addVisitor();

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

	public NumberStruct subtract(final NumberStruct number) {
		final SubtractVisitor<?> subtractVisitor = subtractVisitor();
		return number.subtract(subtractVisitor);
	}

	protected abstract NumberStruct subtract(final SubtractVisitor<?> subtractVisitor);

	protected abstract SubtractVisitor<?> subtractVisitor();

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

	public NumberStruct multiply(final NumberStruct number) {
		final MultiplyVisitor<?> multiplyVisitor = multiplyVisitor();
		return number.multiply(multiplyVisitor);
	}

	protected abstract NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor);

	protected abstract MultiplyVisitor<?> multiplyVisitor();

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

	public NumberStruct divide(final NumberStruct number) {
		final DivideVisitor<?> divideVisitor = divideVisitor();
		return number.divide(divideVisitor);
	}

	protected abstract NumberStruct divide(final DivideVisitor<?> divideVisitor);

	protected abstract DivideVisitor<?> divideVisitor();

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

	public boolean isEqualTo(final NumberStruct number) {
		final EqualToVisitor<?> equalToVisitor = equalToVisitor();
		return number.isEqualTo(equalToVisitor);
	}

	protected abstract boolean isEqualTo(final EqualToVisitor<?> equalToVisitor);

	protected abstract EqualToVisitor<?> equalToVisitor();

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

	public boolean isNotEqualTo(final NumberStruct number) {
		return !isEqualTo(number);
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

	public abstract NumberStruct signum();

	public abstract NumberStruct realPart();

	public abstract NumberStruct imagPart();

	public abstract NumberStruct conjugate();

	public abstract NumberStruct negation();

	public abstract NumberStruct reciprocal();

	public abstract NumberStruct exp();

	public NumberStruct expt(final NumberStruct power) {
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

	protected abstract NumberStruct expt(final ExptVisitor<?> exptVisitor);

	protected abstract ExptVisitor<?> exptVisitor();

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

	// Visitor Implementations

	protected abstract static class AddVisitor<S extends NumberStruct> {

		protected final S number1;

		protected AddVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct add(IntegerStruct number2);

		public abstract NumberStruct add(FloatStruct number2);

		public abstract NumberStruct add(RatioStruct number2);

		public abstract NumberStruct add(ComplexStruct number2);
	}

	protected abstract static class SubtractVisitor<S extends NumberStruct> {

		protected final S number1;

		protected SubtractVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct subtract(IntegerStruct number2);

		public abstract NumberStruct subtract(FloatStruct number2);

		public abstract NumberStruct subtract(RatioStruct number2);

		public abstract NumberStruct subtract(ComplexStruct number2);
	}

	protected abstract static class MultiplyVisitor<S extends NumberStruct> {

		protected final S number1;

		protected MultiplyVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct multiply(IntegerStruct number2);

		public abstract NumberStruct multiply(FloatStruct number2);

		public abstract NumberStruct multiply(RatioStruct number2);

		public abstract NumberStruct multiply(ComplexStruct number2);
	}

	protected abstract static class DivideVisitor<S extends NumberStruct> {

		protected final S number1;

		protected DivideVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract NumberStruct divide(IntegerStruct number2);

		public abstract NumberStruct divide(FloatStruct number2);

		public abstract NumberStruct divide(RatioStruct number2);

		public abstract NumberStruct divide(ComplexStruct number2);
	}

	protected abstract static class EqualToVisitor<S extends NumberStruct> {

		protected final S number1;

		protected EqualToVisitor(final S number1) {
			this.number1 = number1;
		}

		public abstract boolean equalTo(IntegerStruct number2);

		public abstract boolean equalTo(FloatStruct number2);

		public abstract boolean equalTo(RatioStruct number2);

		public abstract boolean equalTo(ComplexStruct number2);
	}

	protected abstract static class ExptVisitor<S extends NumberStruct> {

		protected final S base;

		protected ExptVisitor(final S base) {
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
}

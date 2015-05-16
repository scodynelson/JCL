/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.util.List;

import jcl.LispStruct;
import jcl.classes.BuiltInClassStruct;
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

	public abstract NumberStruct add(NumberStruct number);

	public abstract NumberStruct subtract(NumberStruct number);

	public abstract NumberStruct multiply(NumberStruct number);

	public abstract NumberStruct divide(NumberStruct number);

	public abstract boolean zerop();

	public abstract NumberStruct signum();

	public abstract boolean isEqualTo(LispStruct obj);

	public abstract boolean isNotEqualTo(LispStruct obj);

	public abstract NumberStruct exp();

	public abstract NumberStruct expt(NumberStruct power);

	protected static NumberStruct intexp(final NumberStruct base, final IntegerStruct power) {
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
			return IntegerStruct.ONE.divide(intexp(base, realPower));
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

	public abstract NumberStruct conjugate();

	public abstract NumberStruct negate();

	public abstract NumberStruct sqrt();

	public abstract NumberStruct log();

	public NumberStruct log(final NumberStruct base) {
		return log().divide(base.log());
	}

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

	public abstract NumberStruct realPart();

	public abstract NumberStruct imagPart();
}

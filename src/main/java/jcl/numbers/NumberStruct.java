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
	NumberStruct(final NumberType type,
	             final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	public RealStruct ABS() {
		return null;
	}

	public abstract NumberStruct add(final NumberStruct number);

	public abstract NumberStruct subtract(final NumberStruct number);

	public abstract NumberStruct multiply(final NumberStruct number);

	public abstract NumberStruct divide(final NumberStruct number);

	public abstract boolean zerop();

	public abstract boolean isEqualTo(final LispStruct obj);

	public abstract boolean isNotEqualTo(final LispStruct obj);

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
}

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
public class NumberStruct extends BuiltInClassStruct {

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

	public NumberStruct add(final NumberStruct numberStruct) {
		return null;
	}

	public NumberStruct subtract(final NumberStruct numberStruct) {
		return null;
	}

	public NumberStruct multiply(final NumberStruct numberStruct) {
		return null;
	}

	public NumberStruct divide(final NumberStruct numberStruct) {
		return null;
	}

	public boolean zerop() {
		return false;
	}

	public boolean isEqualTo(final LispStruct obj) {
		return false;
	}

	public boolean isNotEqualTo(final LispStruct obj) {
		return false;
	}

	public boolean isLessThan(final LispStruct obj) {
		return false;
	}

	public boolean isGreaterThan(final LispStruct obj) {
		return false;
	}

	public boolean isLessThanOrEqualTo(final LispStruct obj) {
		return false;
	}

	public boolean isGreaterThanOrEqualTo(final LispStruct obj) {
		return false;
	}
}

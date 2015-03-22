/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.util.List;

import jcl.LispStruct;
import jcl.types.Rational;

/**
 * The {@link RationalStruct} is the object representation of a Lisp 'rational' type.
 */
public class RationalStruct extends RealStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3775544213011392520L;

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected RationalStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(Rational.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the rational object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	RationalStruct(final Rational type,
	               final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.util.List;

import jcl.LispStruct;
import jcl.types.RealType;

/**
 * The {@link RealStruct} is the object representation of a Lisp 'real' type.
 */
public class RealStruct extends NumberStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -7159935653316309907L;

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected RealStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(RealType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the real object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	RealStruct(final RealType type,
	           final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	public boolean plusp() {
		return false;
	}

	public boolean minusp() {
		return false;
	}

	public RationalStruct rational() {
		return null;
	}

	public RationalStruct rationalize() {
		return null;
	}

	public RealStruct MOD(final RealStruct divisor) {
		final RealStruct result = truncate(divisor);
		// TODO: this doesn't return both values...
		if (!result.zerop()) {
			if (divisor.minusp()) {
				if (plusp()) {
					return (RealStruct) result.add(divisor);
				}
			} else {
				if (minusp()) {
					return (RealStruct) result.add(divisor);
				}
			}
		}
		return result;
	}

	public RealStruct truncate(final RealStruct obj) {
		return null;
	}
}

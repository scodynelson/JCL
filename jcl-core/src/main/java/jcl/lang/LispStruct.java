/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import jcl.type.LispType;
import jcl.type.TType;

/**
 * The {@link LispStruct} is the representation for all Lisp types.
 */
public interface LispStruct {

	/**
	 * This method returns the type of the struct.
	 *
	 * @return the type of the struct
	 */
	default LispType getType() {
		return TType.INSTANCE;
	}

	default boolean eq(final LispStruct object) {
		return this == object;
	}

	default boolean eql(final LispStruct object) {
		return eq(object);
	}

	default boolean equal(final LispStruct object) {
		return eql(object);
	}

	default boolean equalp(final LispStruct object) {
		return equal(object);
	}
}

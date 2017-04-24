/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.lang.LispStruct;

/**
 * This interface is the common parent for all lisp types.
 */
public interface LispType extends LispStruct {

	default boolean isOfType(final LispType lispType) {
		return typeEquals(lispType) || lispType.typeEquals(this);
	}

	default boolean isNotOfType(final LispType lispType) {
		return !typeEquals(lispType) && !lispType.typeEquals(this);
	}

	boolean typeEquals(final Object obj);
}

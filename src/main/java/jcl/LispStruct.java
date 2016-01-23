/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl;

import jcl.types.TType;

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
}

package jcl;

import jcl.types.T;

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
		return T.INSTANCE;
	}

	/**
	 * This method returns the printed representation of the struct.
	 *
	 * @return the printed representation of the struct
	 */
	default String printStruct() {
		return toString();
	}
}

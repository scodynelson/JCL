package jcl.structs;

import jcl.LispType;
import jcl.types.StandardMethod;

/**
 * The {@code StandardMethodStruct} is the object representation of a Lisp 'standard-method' type.
 */
public class StandardMethodStruct extends MethodStruct {
	// TODO: Also extends StandardObjectStruct...

	@Override
	public LispType getType() {
		return StandardMethod.INSTANCE;
	}
}

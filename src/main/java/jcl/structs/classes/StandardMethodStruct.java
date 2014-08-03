package jcl.structs.classes;

import jcl.LispType;
import jcl.types.StandardMethod;

/**
 * The {@code StandardMethodStruct} is the object representation of a Lisp 'standard-method' type.
 */
public abstract class StandardMethodStruct extends MethodStruct {
	// TODO: Also extends StandardObjectStruct...

	@Override
	public LispType getType() {
		return StandardMethod.INSTANCE;
	}

	@Override
	public String toString() {
		return "StandardMethodStruct{}";
	}
}

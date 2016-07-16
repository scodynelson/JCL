package jcl.classes;

import jcl.LispType;
import jcl.types.StandardMethodType;

/**
 * The {@link StandardMethodStruct} is the object representation of a Lisp 'standard-method' type.
 */
public abstract class StandardMethodStruct extends MethodStruct {
	// TODO: Also extends StandardObjectStruct...

	@Override
	public LispType getType() {
		return StandardMethodType.INSTANCE;
	}
}

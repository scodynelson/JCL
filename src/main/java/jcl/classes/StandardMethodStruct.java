package jcl.classes;

import jcl.LispType;
import jcl.types.StandardMethod;

/**
 * The {@link StandardMethodStruct} is the object representation of a Lisp 'standard-method' type.
 */
public abstract class StandardMethodStruct extends MethodStruct {
	// TODO: Also extends StandardObjectStruct...

	private static final long serialVersionUID = 6730344897930096724L;

	@Override
	public LispType getType() {
		return StandardMethod.INSTANCE;
	}
}

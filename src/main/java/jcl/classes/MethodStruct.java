package jcl.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.Method;

/**
 * The {@code MethodStruct} is the object representation of a Lisp 'method' type.
 */
public abstract class MethodStruct implements LispStruct {

	@Override
	public LispType getType() {
		return Method.INSTANCE;
	}
}

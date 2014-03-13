package jcl.structs;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.Method;

/**
 * The {@code MethodStruct} is the object representation of a Lisp 'method' type.
 */
public class MethodStruct implements LispStruct {

	@Override
	public LispType getType() {
		return Method.INSTANCE;
	}
}

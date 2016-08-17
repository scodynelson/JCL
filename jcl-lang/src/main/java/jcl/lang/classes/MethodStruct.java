package jcl.lang.classes;

import jcl.lang.LispStruct;
import jcl.type.LispType;
import jcl.type.MethodType;

/**
 * The {@link MethodStruct} is the object representation of a Lisp 'method' type.
 */
public abstract class MethodStruct implements LispStruct {

	@Override
	public LispType getType() {
		return MethodType.INSTANCE;
	}
}

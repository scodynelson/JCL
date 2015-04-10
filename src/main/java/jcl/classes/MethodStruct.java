package jcl.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.MethodType;

/**
 * The {@link MethodStruct} is the object representation of a Lisp 'method' type.
 */
public abstract class MethodStruct implements LispStruct {

	private static final long serialVersionUID = 5505526217809826904L;

	@Override
	public LispType getType() {
		return MethodType.INSTANCE;
	}
}

package jcl.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.MethodCombination;

/**
 * The {@link MethodCombinationStruct} is the object representation of a Lisp 'method-combination' type.
 */
public abstract class MethodCombinationStruct implements LispStruct {

	private static final long serialVersionUID = -6546675847008375630L;

	@Override
	public LispType getType() {
		return MethodCombination.INSTANCE;
	}
}

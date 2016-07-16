package jcl.lang;

import jcl.type.LispType;
import jcl.type.MethodCombinationType;

/**
 * The {@link MethodCombinationStruct} is the object representation of a Lisp 'method-combination' type.
 */
public abstract class MethodCombinationStruct implements LispStruct {

	@Override
	public LispType getType() {
		return MethodCombinationType.INSTANCE;
	}
}

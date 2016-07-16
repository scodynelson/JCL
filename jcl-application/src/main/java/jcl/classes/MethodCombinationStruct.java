package jcl.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.MethodCombinationType;

/**
 * The {@link MethodCombinationStruct} is the object representation of a Lisp 'method-combination' type.
 */
public abstract class MethodCombinationStruct implements LispStruct {

	@Override
	public LispType getType() {
		return MethodCombinationType.INSTANCE;
	}
}

package jcl.structs.classes;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.classes.MethodCombination;

/**
 * The {@code MethodCombinationStruct} is the object representation of a Lisp 'method-combination' type.
 */
public class MethodCombinationStruct implements LispStruct {

	@Override
	public LispType getType() {
		return MethodCombination.INSTANCE;
	}
}

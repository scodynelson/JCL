package jcl.structs.classes;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.classes.MethodCombination;

public class MethodCombinationStruct implements LispStruct {

	@Override
	public LispType getType() {
		return MethodCombination.INSTANCE;
	}
}

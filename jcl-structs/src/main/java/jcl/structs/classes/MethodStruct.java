package jcl.structs.classes;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.classes.Method;

public class MethodStruct implements LispStruct {

	@Override
	public LispType getType() {
		return Method.INSTANCE;
	}
}

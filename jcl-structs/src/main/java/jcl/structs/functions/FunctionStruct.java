package jcl.structs.functions;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.functions.Function;

public class FunctionStruct implements LispStruct {

	@Override
	public LispType getType() {
		return Function.INSTANCE;
	}

	public LispStruct funcall(LispStruct... lispStructs) {
		return null;
	}
}

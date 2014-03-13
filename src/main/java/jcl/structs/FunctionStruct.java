package jcl.structs;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.Function;

public class FunctionStruct implements LispStruct {

	@Override
	public LispType getType() {
		return Function.INSTANCE;
	}

	public LispStruct funcall(final LispStruct... lispStructs) {
		return null;
	}
}

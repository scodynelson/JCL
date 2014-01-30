package jcl.structs;

import jcl.types.LispType;
import jcl.types.NIL;

public class NILStruct implements LispStruct {

	public static final NILStruct INSTANCE = new NILStruct();

	private NILStruct() {
	}

	@Override
	public LispType getType() {
		return NIL.INSTANCE;
	}
}

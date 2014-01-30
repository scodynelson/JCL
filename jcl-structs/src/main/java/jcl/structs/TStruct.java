package jcl.structs;

import jcl.types.LispType;
import jcl.types.T;

public class TStruct implements LispStruct {

	public static final TStruct INSTANCE = new TStruct();

	private TStruct() {
	}

	@Override
	public LispType getType() {
		return T.INSTANCE;
	}
}

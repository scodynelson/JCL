package jcl.structs;

import jcl.structs.classes.BuiltInClassStruct;
import jcl.types.T;

public class TStruct extends BuiltInClassStruct {

	public static final TStruct INSTANCE = new TStruct();

	private TStruct() {
		super(T.INSTANCE, null, null);
	}
}

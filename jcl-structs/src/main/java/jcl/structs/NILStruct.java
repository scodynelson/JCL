package jcl.structs;

import jcl.structs.classes.BuiltInClassStruct;
import jcl.types.NIL;

public class NILStruct extends BuiltInClassStruct {

	public static final NILStruct INSTANCE = new NILStruct();

	private NILStruct() {
		super(NIL.INSTANCE, null, null);
	}
}

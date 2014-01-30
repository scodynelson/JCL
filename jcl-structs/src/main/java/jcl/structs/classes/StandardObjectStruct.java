package jcl.structs.classes;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.classes.StandardObject;

public class StandardObjectStruct implements LispStruct {

	@Override
	public LispType getType() {
		return StandardObject.INSTANCE;
	}
}

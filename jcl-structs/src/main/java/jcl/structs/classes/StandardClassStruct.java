package jcl.structs.classes;

import jcl.types.LispType;
import jcl.types.classes.StandardClass;

public class StandardClassStruct extends ClassStruct {

	@Override
	public LispType getType() {
		return StandardClass.INSTANCE;
	}
}

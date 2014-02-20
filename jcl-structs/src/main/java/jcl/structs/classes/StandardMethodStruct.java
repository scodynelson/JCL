package jcl.structs.classes;

import jcl.types.LispType;
import jcl.types.classes.StandardMethod;

public class StandardMethodStruct extends MethodStruct {
	// TODO: Also extends StandardObjectStruct...

	@Override
	public LispType getType() {
		return StandardMethod.INSTANCE;
	}
}

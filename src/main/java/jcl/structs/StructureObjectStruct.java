package jcl.structs;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.StructureObject;

/**
 * The {@code StructureObjectStruct} is the object representation of a Lisp 'structure-object' type.
 */
public class StructureObjectStruct implements LispStruct {

	@Override
	public LispType getType() {
		return StructureObject.INSTANCE;
	}
}

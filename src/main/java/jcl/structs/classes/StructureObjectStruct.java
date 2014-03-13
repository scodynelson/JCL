package jcl.structs.classes;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.classes.StructureObject;

/**
 * The {@code StructureObjectStruct} is the object representation of a Lisp 'structure-object' type.
 */
public class StructureObjectStruct implements LispStruct {

	@Override
	public LispType getType() {
		return StructureObject.INSTANCE;
	}
}

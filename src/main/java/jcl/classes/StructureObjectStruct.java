package jcl.classes;

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

	@Override
	public String printStruct() {
		final String typeClassName = getType().getClass().getName().toUpperCase();
		return "#<" + typeClassName + '>';
	}
}

package jcl.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.StructureObject;

/**
 * The {@link StructureObjectStruct} is the object representation of a Lisp 'structure-object' type.
 */
public class StructureObjectStruct implements LispStruct {

	private static final long serialVersionUID = 5766790087319221572L;

	@Override
	public LispType getType() {
		return StructureObject.INSTANCE;
	}
}

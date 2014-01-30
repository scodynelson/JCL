package jcl.structs.classes;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.classes.StructureObject;

public class StructureObjectStruct implements LispStruct {

	/*
	(defstruct (structure-object (:alternate-metaclass instance)))
	 */

	@Override
	public LispType getType() {
		return StructureObject.INSTANCE;
	}
}

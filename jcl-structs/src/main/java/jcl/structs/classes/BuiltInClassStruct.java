package jcl.structs.classes;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.classes.BuiltInClass;

import java.util.List;

public class BuiltInClassStruct extends ClassStruct {

	public BuiltInClassStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(BuiltInClass.INSTANCE, directSuperClasses, subClasses);
	}

	protected BuiltInClassStruct(final LispType type,
								 final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}
}

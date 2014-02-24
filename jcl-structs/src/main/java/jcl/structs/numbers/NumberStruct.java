package jcl.structs.numbers;

import jcl.structs.LispStruct;
import jcl.structs.classes.BuiltInClassStruct;
import jcl.types.numbers.Number;

import java.util.List;

/**
 * The {@code NumberStruct} is the object representation of a Lisp 'number' type.
 */
public class NumberStruct extends BuiltInClassStruct {

	protected NumberStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(Number.INSTANCE, directSuperClasses, subClasses);
	}

	protected NumberStruct(final Number type,
						   final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}
}

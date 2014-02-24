package jcl.structs.numbers;

import jcl.structs.LispStruct;
import jcl.types.numbers.Real;

import java.util.List;

/**
 * The {@code RealStruct} is the object representation of a Lisp 'real' type.
 */
public class RealStruct extends NumberStruct {

	protected RealStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(Real.INSTANCE, directSuperClasses, subClasses);
	}

	protected RealStruct(final Real type,
						 final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}
}

package jcl.structs.numbers;

import jcl.structs.LispStruct;
import jcl.types.numbers.Real;

import java.util.List;

/**
 * The {@code RealStruct} is the object representation of a Lisp 'real' type.
 */
public class RealStruct extends NumberStruct {

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected RealStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(Real.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type               the type of the real object
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected RealStruct(final Real type,
						 final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	@Override
	public String toString() {
		return "RealStruct{}";
	}
}

package jcl.numbers;

import jcl.LispStruct;
import jcl.classes.BuiltInClassStruct;
import jcl.types.Number;

import java.util.List;

/**
 * The {@link NumberStruct} is the object representation of a Lisp 'number' type.
 */
public class NumberStruct extends BuiltInClassStruct {

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected NumberStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(Number.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type               the type of the number object
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected NumberStruct(final Number type,
	                       final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	@Override
	public String toString() {
		return "NumberStruct{}";
	}
}

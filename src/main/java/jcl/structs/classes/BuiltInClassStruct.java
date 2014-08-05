package jcl.structs.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.BuiltInClass;

import java.util.List;

/**
 * The {@link BuiltInClassStruct} is the object representation of a Lisp 'built-in-class' type.
 */
public abstract class BuiltInClassStruct extends ClassStruct {

	/**
	 * Public constructor.
	 *
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected BuiltInClassStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this(BuiltInClass.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type               the type of the class object
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected BuiltInClassStruct(final LispType type,
	                             final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	@Override
	public String toString() {
		return "BuiltInClassStruct{}";
	}
}

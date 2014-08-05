package jcl.structs.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.StandardClass;

import java.util.List;

/**
 * The {@link StandardClassStruct} is the object representation of a Lisp 'standard-class' type.
 */
public abstract class StandardClassStruct extends ClassStruct {

	/**
	 * Public constructor.
	 *
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected StandardClassStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this(StandardClass.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type               the type of the class object
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected StandardClassStruct(final LispType type,
	                              final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	@Override
	public String toString() {
		return "StandardClassStruct{}";
	}
}

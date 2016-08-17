package jcl.lang.classes;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.type.LispType;
import jcl.type.StandardClassType;

/**
 * The {@link StandardClassStruct} is the object representation of a Lisp 'standard-class' type.
 */
public abstract class StandardClassStruct extends ClassStruct {

	/**
	 * Public constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected StandardClassStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(StandardClassType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the class object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected StandardClassStruct(final LispType type,
	                              final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}
}

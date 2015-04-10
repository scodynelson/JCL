package jcl.classes;

import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.StandardClassType;

/**
 * The {@link StandardClassStruct} is the object representation of a Lisp 'standard-class' type.
 */
public abstract class StandardClassStruct extends ClassStruct {

	private static final long serialVersionUID = 2139719428649639922L;

	/**
	 * Public constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected StandardClassStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
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
	                              final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}
}

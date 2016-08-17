package jcl.lang.classes;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.type.BuiltInClassType;
import jcl.type.LispType;

/**
 * The {@link BuiltInClassStruct} is the object representation of a Lisp 'built-in-class' type.
 */
public abstract class BuiltInClassStruct extends ClassStruct {

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected BuiltInClassStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(null, BuiltInClassType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected BuiltInClassStruct(final String documentation,
	                             final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(documentation, BuiltInClassType.INSTANCE, directSuperClasses, subClasses);
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
	protected BuiltInClassStruct(final LispType type,
	                             final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(null, type, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 * @param type
	 * 		the type of the class object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected BuiltInClassStruct(final String documentation, final LispType type,
	                             final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(documentation, type, directSuperClasses, subClasses);
	}
}

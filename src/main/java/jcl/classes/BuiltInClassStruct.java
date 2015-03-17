package jcl.classes;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.BuiltInClass;

import java.util.List;

/**
 * The {@link BuiltInClassStruct} is the object representation of a Lisp 'built-in-class' type.
 */
public abstract class BuiltInClassStruct extends ClassStruct {

	private static final long serialVersionUID = -4998279729528734323L;

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected BuiltInClassStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this(null, BuiltInClass.INSTANCE, directSuperClasses, subClasses);
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
	                             final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this(documentation, BuiltInClass.INSTANCE, directSuperClasses, subClasses);
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
	                             final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(null, type, directSuperClasses, subClasses);
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
	                             final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(documentation, type, directSuperClasses, subClasses);
	}
}

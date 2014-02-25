package jcl.structs.classes;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.classes.StandardClass;

import java.util.List;

/**
 * The {@code StandardClassStruct} is the object representation of a Lisp 'standard-class' type.
 */
public class StandardClassStruct extends ClassStruct {

	/**
	 * Public constructor.
	 *
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	public StandardClassStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
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
}

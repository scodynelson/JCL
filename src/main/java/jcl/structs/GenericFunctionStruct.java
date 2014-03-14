package jcl.structs;

import jcl.types.GenericFunction;
import jcl.types.LispType;

import java.util.List;

/**
 * The {@code GenericFunctionStruct} is the object representation of a Lisp 'generic-function' type.
 */
public abstract class GenericFunctionStruct extends FunctionStruct {

	/**
	 * Protected constructor.
	 */
	protected GenericFunctionStruct() {
		this(null, null);
	}

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected GenericFunctionStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this(GenericFunction.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type               the type of the generic-function object
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected GenericFunctionStruct(final LispType type,
									final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}
}

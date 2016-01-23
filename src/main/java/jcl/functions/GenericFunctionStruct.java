package jcl.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.GenericFunctionType;

/**
 * The {@link GenericFunctionStruct} is the object representation of a Lisp 'generic-function' type.
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
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected GenericFunctionStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(GenericFunctionType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the generic-function object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected GenericFunctionStruct(final LispType type,
	                                final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}
}

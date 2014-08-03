package jcl.structs.functions;

import jcl.LispStruct;
import jcl.types.StandardGenericFunction;

import java.util.List;

/**
 * The {@code StandardGenericFunctionStruct} is the object representation of a Lisp 'standard-generic-function' type.
 */
public abstract class StandardGenericFunctionStruct extends GenericFunctionStruct {

	/**
	 * Protected constructor.
	 */
	protected StandardGenericFunctionStruct() {
		this(null, null);
	}

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected StandardGenericFunctionStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(StandardGenericFunction.INSTANCE, directSuperClasses, subClasses);
	}
}

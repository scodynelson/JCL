package jcl.functions;

import jcl.LispStruct;
import jcl.types.StandardGenericFunction;

import java.util.List;

/**
 * The {@link StandardGenericFunctionStruct} is the object representation of a Lisp 'standard-generic-function' type.
 */
public abstract class StandardGenericFunctionStruct extends GenericFunctionStruct {

	private static final long serialVersionUID = -7062120296932518329L;

	/**
	 * Protected constructor.
	 */
	protected StandardGenericFunctionStruct() {
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
	protected StandardGenericFunctionStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(StandardGenericFunction.INSTANCE, directSuperClasses, subClasses);
	}
}

package jcl.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.types.StandardGenericFunctionType;

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
	protected StandardGenericFunctionStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(StandardGenericFunctionType.INSTANCE, directSuperClasses, subClasses);
	}
}

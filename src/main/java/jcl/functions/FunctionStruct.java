package jcl.functions;

import jcl.LispStruct;
import jcl.classes.BuiltInClassStruct;
import jcl.types.Function;
import jcl.LispType;

import java.util.List;

/**
 * The {@code FunctionStruct} is the object representation of a Lisp 'function' type.
 */
public abstract class FunctionStruct extends BuiltInClassStruct {

	/**
	 * Protected constructor.
	 */
	protected FunctionStruct() {
		this(null, null);
	}

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected FunctionStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this(Function.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type               the type of the function object
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected FunctionStruct(final LispType type,
							 final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	@Override
	public String printStruct() {
		final String typeClassName = getType().getClass().getName().toUpperCase();
		return "#<" + typeClassName + '>';
	}

	/**
	 * This is the application method for any function structure.
	 *
	 * @param lispStructs the function arguments
	 * @return the result object
	 */
	public abstract LispStruct apply(LispStruct... lispStructs);
}

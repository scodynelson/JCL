package jcl.structs.functions;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.GenericFunction;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

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
	protected GenericFunctionStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this(GenericFunction.INSTANCE, directSuperClasses, subClasses);
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
	                                final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}

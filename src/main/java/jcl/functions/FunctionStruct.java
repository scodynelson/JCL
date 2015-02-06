package jcl.functions;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.classes.BuiltInClassStruct;
import jcl.types.Function;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

/**
 * The {@link FunctionStruct} is the object representation of a Lisp 'function' type.
 */
public abstract class FunctionStruct extends BuiltInClassStruct {

	private static final long serialVersionUID = 7356724806391677112L;

	private OrdinaryLambdaListBindings lambdaListBindings;

	/**
	 * Protected constructor.
	 */
	protected FunctionStruct() {
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
	protected FunctionStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this(Function.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the function object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected FunctionStruct(final LispType type,
	                         final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	/**
	 * This is the application method for any function structure.
	 *
	 * @param lispStructs
	 * 		the function arguments
	 *
	 * @return the result object
	 */
	public abstract LispStruct apply(LispStruct... lispStructs);

	public OrdinaryLambdaListBindings getLambdaListBindings() {
		return lambdaListBindings;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}

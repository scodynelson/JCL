package jcl.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import jcl.classes.BuiltInClassStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.types.Function;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link FunctionStruct} is the object representation of a Lisp 'function' type.
 */
public abstract class FunctionStruct extends BuiltInClassStruct {

	private static final long serialVersionUID = 7356724806391677112L;

	protected OrdinaryLambdaListBindings lambdaListBindings;

	/**
	 * Protected constructor.
	 */
	protected FunctionStruct() {
		this(null, Function.INSTANCE, null, null);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 */
	protected FunctionStruct(final String documentation) {
		this(documentation, null);
	}

	/**
	 * Protected constructor.
	 *
	 * @param lambdaListBindings
	 * 		lambda-list bindings for the function
	 */
	protected FunctionStruct(final OrdinaryLambdaListBindings lambdaListBindings) {
		this(null, lambdaListBindings);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 * @param lambdaListBindings
	 * 		lambda-list bindings for the function
	 */
	protected FunctionStruct(final String documentation, final OrdinaryLambdaListBindings lambdaListBindings) {
		this(documentation, Function.INSTANCE, null, null);
		this.lambdaListBindings = lambdaListBindings;
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
		this(null, Function.INSTANCE, directSuperClasses, subClasses);
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
	protected FunctionStruct(final String documentation,
	                         final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this(documentation, Function.INSTANCE, directSuperClasses, subClasses);
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
		this(null, type, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 * @param type
	 * 		the type of the function object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected FunctionStruct(final String documentation, final LispType type,
	                         final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(documentation, type, directSuperClasses, subClasses);
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

	public void setLambdaListBindings(final OrdinaryLambdaListBindings lambdaListBindings) {
		this.lambdaListBindings = lambdaListBindings;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(lambdaListBindings)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final FunctionStruct rhs = (FunctionStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(lambdaListBindings, rhs.lambdaListBindings)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(lambdaListBindings)
		                                                                .toString();
	}
}

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link GenericFunction} is a {@link Function} whose behavior depends on the {@link Class}es or identities of the
 * arguments supplied to it. A {@link GenericFunction} object contains a set of {@link Method}s, a lambda list, a
 * {@link MethodCombination} type, and other information. The {@link Method}s define the class-specific behavior and
 * operations of the {@link GenericFunction}; a {@link Method} is said to specialize a {@link GenericFunction}. When
 * invoked, a {@link GenericFunction} executes a subset of its {@link Method}s based on the {@link Class}es or
 * identities of its arguments.
 * <p>
 * A {@link GenericFunction} can be used in the same ways that an ordinary {@link Function} can be used.
 * <p>
 * {@link GenericFunction} -> {@link Function} -> {@link T}
 */
public interface GenericFunction extends Function {

	GenericFunction INSTANCE = new Factory.GenericFunctionImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<GenericFunction> {

		@Override
		public GenericFunction getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link GenericFunction} type implementation.
		 */
		private static final class GenericFunctionImpl extends TypeBaseClass implements GenericFunction, AtomicTypeSpecifier {

			private static final long serialVersionUID = 2621198636561454203L;

			/**
			 * Private constructor.
			 */
			private GenericFunctionImpl() {
				super("GENERIC-FUNCTION");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof GenericFunction);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

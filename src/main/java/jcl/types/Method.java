package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link Method} is an object that represents a modular part of the behavior of a {@link GenericFunction}.
 * <p>
 * A {@link Method} contains code to implement the {@link Method}'s behavior, a sequence of parameter specializers that
 * specify when the given {@link Method} is applicable, and a sequence of qualifiers that is used by the {@link
 * MethodCombination} facility to distinguish among {@link Method}s. Each required parameter of each {@link Method} has
 * an associated parameter specializer, and the {@link Method} will be invoked only on arguments that satisfy its
 * parameter specializers.
 * <p>
 * The {@link MethodCombination} facility controls the selection of {@link Method}s, the order in which they are run,
 * and the values that are returned by the {@link GenericFunction}. The object system offers a default {@link
 * MethodCombination} type and provides a facility for declaring new types of {@link MethodCombination}.
 * <p>
 * {@link Method} -> {@link T}
 */
public interface Method extends T {

	Method INSTANCE = new Factory.MethodImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Method> {

		@Override
		public Method getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Method} type implementation.
		 */
		private static class MethodImpl extends TypeBaseClass implements Method, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private MethodImpl() {
				super("METHOD", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Method);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

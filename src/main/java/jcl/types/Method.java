package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Method} is an object that represents a modular part of the behavior of a {@code GenericFunction}.
 * <p/>
 * A {@code Method} contains code to implement the {@code Method}'s behavior, a sequence of parameter specializers that
 * specify when the given {@code Method} is applicable, and a sequence of qualifiers that is used by the {@code MethodCombination}
 * facility to distinguish among {@code Method}s. Each required parameter of each {@code Method} has an associated parameter
 * specializer, and the {@code Method} will be invoked only on arguments that satisfy its parameter specializers.
 * <p/>
 * The {@code MethodCombination} facility controls the selection of {@code Method}s, the order in which they are run,
 * and the values that are returned by the {@code GenericFunction}. The object system offers a default {@code MethodCombination}
 * type and provides a facility for declaring new types of {@code MethodCombination}.
 * <p/>
 * {@code Method} -> {@code T}
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
		 * Inner {@code Method} type implementation.
		 */
		private static class MethodImpl implements Method, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Method);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

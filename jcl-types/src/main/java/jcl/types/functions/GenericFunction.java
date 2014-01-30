package jcl.types.functions;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code GenericFunction} is a {@code Function} whose behavior depends on the {@code Class}es or identities of the
 * arguments supplied to it. A {@code GenericFunction} object contains a set of {@code Method}s, a lambda list, a
 * {@code MethodCombination} type, and other information. The {@code Method}s define the class-specific behavior and
 * operations of the {@code GenericFunction}; a {@code Method} is said to specialize a {@code GenericFunction}. When
 * invoked, a {@code GenericFunction} executes a subset of its {@code Method}s based on the {@code Class}es or identities
 * of its arguments.
 * <p/>
 * A {@code GenericFunction} can be used in the same ways that an ordinary {@code Function} can be used.
 */
public interface GenericFunction extends Function, T {

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
		 * Inner {@code GenericFunction} type implementation.
		 */
		private static class GenericFunctionImpl implements GenericFunction, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof GenericFunction)) {
					return false;
				}

				final GenericFunction genericFunction = (GenericFunction) obj;
				return genericFunction == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code MethodCombination} object represents the information about the {@code MethodCombination} being used by a
 * {@code GenericFunction}. A {@code MethodCombination} object contains information about both the type of {@code MethodCombination}
 * and the arguments being used with that type.
 * <p/>
 * {@code MethodCombination} -> {@code T}
 */
public interface MethodCombination extends T {

	MethodCombination INSTANCE = new Factory.MethodCombinationImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<MethodCombination> {

		@Override
		public MethodCombination getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code MethodCombination} type implementation.
		 */
		private static class MethodCombinationImpl implements MethodCombination, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof MethodCombination);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
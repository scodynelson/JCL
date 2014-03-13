package jcl.types.numbers;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code RandomState} object contains state information used by the pseudo-random number generator.
 * <p/>
 * {@code RandomState} -> {@code T}
 */
public interface RandomState extends T {

	RandomState INSTANCE = new Factory.RandomStateImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<RandomState> {

		@Override
		public RandomState getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code RandomState} type implementation.
		 */
		private static class RandomStateImpl implements RandomState, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof RandomState);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

package jcl.types.conditions;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Restart} represents a {@code Function} that can be called to perform some form of recovery action, usually a
 * transfer of control to an outer point in the running program.
 * <p/>
 * A {@code Restart} has only dynamic extent relative to the scope of the binding form which establishes it.
 * <p/>
 * {@code Restart} -> {@code T}
 */
public interface Restart extends T {

	Restart INSTANCE = new Factory.RestartImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Restart> {

		@Override
		public Restart getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Restart} type implementation.
		 */
		private static class RestartImpl implements Restart, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Restart);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

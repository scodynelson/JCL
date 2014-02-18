package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@code T} is the set of all objects. It is a supertype of every type, including itself. Every object is of
 * type {@code T}.
 * <p/>
 * {@code T}
 */
public interface T extends LispType {

	T INSTANCE = new Factory.TImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<T> {

		@Override
		public T getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code T} type implementation.
		 */
		private static class TImpl implements T, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof T)) {
					return false;
				}

				final T t = (T) obj;
				return t == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

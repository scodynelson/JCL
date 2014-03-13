package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Sequence} is an ordered collection of objects, called the elements of the sequence. The type {@code Vector}
 * and the type {@code List} are disjoint subtypes of type {@code Sequence}, but are not necessarily an exhaustive partition
 * of the {@code Sequence}. When viewing a {@code Vector} as a {@code Sequence}, only the active elements of that {@code Vector}
 * are considered elements of the {@code Sequence}; that is, {@code Sequence} operations respect the fill pointer when
 * the given {@code Sequence} represents a {@code Vector}.
 * <p/>
 * {@code Sequence} -> {@code T}
 */
public interface Sequence extends T {

	Sequence INSTANCE = new Factory.SequenceImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Sequence> {

		@Override
		public Sequence getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Sequence} type implementation.
		 */
		private static class SequenceImpl implements Sequence, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Sequence);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The only object of type {@code Null} is {@code NIL}, which represents the empty {@code List} and can also be notated ().
 * <p/>
 * {@code Null} -> {@code Symbol} -> {@code List} -> {@code Sequence} -> {@code T}
 */
public interface Null extends Symbol, List {

	Null INSTANCE = new Factory.NullImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Null> {

		@Override
		public Null getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Null} type implementation.
		 */
		private static class NullImpl implements Null, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Null) || (obj instanceof NIL); //NOSONAR
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

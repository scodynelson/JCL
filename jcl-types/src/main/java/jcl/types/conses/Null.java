package jcl.types.conses;

import jcl.types.NIL;
import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.sequences.Sequence;
import jcl.types.symbols.Symbol;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The only object of type {@code Null} is {@code NIL}, which represents the empty {@code List} and can also be notated ().
 */
public interface Null extends Symbol, List, Sequence, T {

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
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Null)) {
					return false;
				}

				final Null aNull = (Null) obj;
				return (aNull == INSTANCE) || (aNull == NIL.INSTANCE);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

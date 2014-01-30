package jcl.types.readtables;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Readtable} maps characters into syntax types for the Lisp reader. A {@code Readtable} also contains associations
 * between macro characters and their reader macro functions, and records information about the case conversion rules to
 * be used by the Lisp reader when parsing {@code Symbol}s.
 * <p/>
 * Each simple {@code Character} must be representable in the {@code Readtable}.
 */
public interface Readtable extends T {

	Readtable INSTANCE = new Factory.ReadtableImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Readtable> {

		@Override
		public Readtable getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Readtable} type implementation.
		 */
		private static class ReadtableImpl implements Readtable, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Readtable)) {
					return false;
				}

				final Readtable readtable = (Readtable) obj;
				return readtable == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

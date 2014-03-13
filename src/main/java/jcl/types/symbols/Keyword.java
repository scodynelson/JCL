package jcl.types.symbols;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@code Keyword} includes all {@code Symbol}s interned the KEYWORD {@code Package}.
 * <p/>
 * Interning a {@code Symbol} in the KEYWORD {@code Package} has three automatic effects:
 * 1. It causes the {@code Symbol} to become bound to itself.
 * 2. It causes the {@code Symbol} to become an external symbol of the KEYWORD package.
 * 3. It causes the {@code Symbol} to become a constant variable.
 * <p/>
 * {@code Keyword} -> {@code Symbol} -> {@code T}
 */
public interface Keyword extends Symbol {

	Keyword INSTANCE = new Factory.KeywordImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Keyword> {

		@Override
		public Keyword getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Keyword} type implementation.
		 */
		private static class KeywordImpl implements Keyword, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Keyword);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

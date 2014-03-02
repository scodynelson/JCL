package jcl.types.characters;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@code StandardChar} is fixed set of 96 characters.
 * <p/>
 * Any character that is not simple is not a standard character.
 * <p/>
 * {@code StandardChar} -> {@code BaseChar} -> {@code Character} -> {@code T}
 */
public interface StandardChar extends BaseChar {

	StandardChar INSTANCE = new Factory.StandardCharImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardChar> {

		@Override
		public StandardChar getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code StandardChar} type implementation.
		 */
		private static class StandardCharImpl implements StandardChar, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardChar);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

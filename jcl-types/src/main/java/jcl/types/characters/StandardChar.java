package jcl.types.characters;

import jcl.types.LispType;
import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@code StandardChar} is fixed set of 96 characters.
 * <p/>
 * Any character that is not simple is not a standard character.
 */
public interface StandardChar extends BaseChar, Character, T {

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
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof LispType)) {
					return false;
				}

				final StandardChar standardChar = (StandardChar) obj;
				return standardChar == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

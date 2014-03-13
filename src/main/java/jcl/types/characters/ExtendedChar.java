package jcl.types.characters;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@code ExtendedChar} is equivalent to the type (and character (not base-char)).
 * <p/>
 * {@code ExtendedChar} -> {@code Character} -> {@code T}
 */
public interface ExtendedChar extends Character {

	ExtendedChar INSTANCE = new Factory.ExtendedCharImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ExtendedChar> {

		@Override
		public ExtendedChar getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code ExtendedChar} type implementation.
		 */
		private static class ExtendedCharImpl implements ExtendedChar, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof ExtendedChar);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Character} is an object that represents a unitary token in an aggregate quantity of text.
 * <p/>
 * The types {@code BaseChar} and {@code ExtendedChar} form an exhaustive partition of the type {@code Character}.
 * <p/>
 * {@code Character} -> {@code T}
 */
public interface Character extends T {

	Character INSTANCE = new Factory.CharacterImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Character> {

		@Override
		public Character getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Character} type implementation.
		 */
		private static class CharacterImpl implements Character, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Character);

			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
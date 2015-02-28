package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link Character} is an object that represents a unitary token in an aggregate quantity of text.
 * <p>
 * The types {@link BaseChar} and {@link ExtendedChar} form an exhaustive partition of the type {@link Character}.
 * <p>
 * {@link Character} -> {@link T}
 */
public interface Character extends T {

	/**
	 * Singleton instance of the {@link Character} type.
	 */
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
		 * Inner {@link Character} type implementation.
		 */
		private static final class CharacterImpl extends TypeBaseClass implements Character, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 8069574443377375137L;

			/**
			 * Private constructor.
			 */
			private CharacterImpl() {
				super("CHARACTER");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Character);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}

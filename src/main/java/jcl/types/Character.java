package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
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
		private static class CharacterImpl extends TypeBaseClass implements Character, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private CharacterImpl() {
				super("CHARACTER", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Character);

			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "CharacterImpl{}";
			}
		}
	}
}

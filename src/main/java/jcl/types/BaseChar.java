package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@code BaseChar} is defined as the upgraded array element type of {@code StandardChar}.
 * <p/>
 * Base characters are distinguished in the following respects:
 * 1. The type {@code StandardChar} is a subrepertoire of the type {@code BaseChar}.
 * 2. The selection of base characters that are not standard characters is implementation defined.
 * 3. Only objects of the type {@code BaseChar} can be elements of a base string.
 * 4. No upper bound is specified for the number of characters in the {@code BaseChar} repertoire. The lower bound
 * is 96, the number of standard characters.
 * <p/>
 * The type {@code StandardChar} is a subtype of type {@code BaseChar}.
 * <p/>
 * {@code BaseChar} -> {@code Character} -> {@code T}
 */
public interface BaseChar extends Character {

	BaseChar INSTANCE = new Factory.BaseCharImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BaseChar> {

		@Override
		public BaseChar getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code BaseChar} type implementation.
		 */
		private static class BaseCharImpl implements BaseChar, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof BaseChar);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

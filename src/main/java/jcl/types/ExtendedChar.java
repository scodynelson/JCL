package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * The type {@link ExtendedChar} is equivalent to the type (and character (not base-char)).
 * <p>
 * {@link ExtendedChar} -> {@link Character} -> {@link T}
 */
public interface ExtendedChar extends Character {

	/**
	 * Singleton instance of the {@link ExtendedChar} type.
	 */
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
		 * Inner {@link ExtendedChar} type implementation.
		 */
		private static final class ExtendedCharImpl extends TypeBaseClass implements ExtendedChar, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -5215210339292683845L;

			/**
			 * Private constructor.
			 */
			private ExtendedCharImpl() {
				super("EXTENDED-CHAR");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof ExtendedChar);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}

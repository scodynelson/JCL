package jcl.types;

import jcl.LispType;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.Integer;

/**
 * A {@code String} is a specialized {@code Vector} whose elements are of type {@code Character} or a subtype of type
 * {@code Character}. When used as a type specifier for object creation, {@code String} means (vector character).
 * <p/>
 * {@code String} -> {@code Vector} -> {@code Array} -> {@code Sequence} -> {@code T}
 */
public interface String extends Vector {

	String INSTANCE = new Factory.StringImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<String> {

		@Override
		public String getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code String} type.
		 *
		 * @param size the size of the {@code String}
		 * @return the newly created compound {@code String} type
		 */
		public static String getInstance(final Integer size) {
			return StringImpl.getInstance(size);
		}

		/**
		 * Inner {@code String} type implementation.
		 */
		private static class StringImpl implements String, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private static final LispType ELEMENT_TYPE = Character.INSTANCE;

			/**
			 * Private constructor.
			 */
			private StringImpl() {
				size = null;
			}

			/**
			 * Private constructor for compound {@code String} type.
			 *
			 * @param size the size of the {@code String}
			 */
			private StringImpl(final Integer size) {
				this.size = new DimensionsDesignator(size);
			}

			@Override
			public DimensionsDesignator getDimensions() {
				return size;
			}

			@Override
			public LispType getElementType() {
				return ELEMENT_TYPE;
			}

			/**
			 * Gets instance of compound {@code String} type.
			 *
			 * @param size the size of the {@code String}
			 * @return the newly created compound {@code String} type
			 */
			public static String getInstance(final Integer size) {
				return new StringImpl(size);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof String)) {
					return false;
				}

				final String aString = (String) obj;
				if (aString == INSTANCE) {
					return true;
				}

				if (aString instanceof StringImpl) {
					final StringImpl aStringImpl = (StringImpl) aString;

					return (size == null) || size.equals(aStringImpl.size);
				}

				return TypeUtils.isArrayLispTypeEqual(this, aString);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder()
						.append(size)
						.append(ELEMENT_TYPE)
						.toHashCode();
			}

			@Override
			public java.lang.String toString() {
				return "StringImpl{"
						+ "size=" + size
						+ ", elementType=" + ELEMENT_TYPE
						+ '}';
			}
		}
	}
}

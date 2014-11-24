package jcl.types;

import jcl.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.Integer;

/**
 * A {@link String} is a specialized {@link Vector} whose elements are of type {@link Character} or a subtype of type
 * {@link Character}. When used as a type specifier for object creation, {@link String} means (vector character).
 * <p>
 * {@link String} -> {@link Vector} -> {@link Array} -> {@link Sequence} -> {@link T}
 */
public interface String extends Vector {

	String INSTANCE = new Factory.StringImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<String> {

		/**
		 * Gets instance of compound {@link String} type.
		 *
		 * @param size
		 * 		the size of the {@link String}
		 *
		 * @return the newly created compound {@link String} type
		 */
		public static String getInstance(final Integer size) {
			return StringImpl.getInstance(size);
		}

		@Override
		public String getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link String} type implementation.
		 */
		private static final class StringImpl extends TypeBaseClass implements String, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private static final LispType ELEMENT_TYPE = Character.INSTANCE;
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private StringImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link String} type.
			 *
			 * @param size
			 * 		the size of the {@link String}
			 */
			private StringImpl(final Integer size) {
				super("STRING");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link String} type.
			 *
			 * @param size
			 * 		the size of the {@link String}
			 *
			 * @return the newly created compound {@link String} type
			 */
			public static String getInstance(final Integer size) {
				return new StringImpl(size);
			}

			@Override
			public DimensionsDesignator getDimensions() {
				return size;
			}

			@Override
			public LispType getElementType() {
				return ELEMENT_TYPE;
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
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
			public java.lang.String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

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
 * A {@link SimpleString} is a specialized one-dimensional {@link SimpleArray} whose elements are of type
 * {@link Character} or a subtype of type {@link Character}. When used as a type specifier for object creation,
 * {@link SimpleString} means (simple-array character (size)).
 * <p>
 * {@link SimpleString} -> {@link String} -> {@link Vector} -> {@link SimpleArray} -> {@link Array} -> {@link Sequence}
 * -> {@link T}
 */
public interface SimpleString extends String, SimpleArray {

	SimpleString INSTANCE = new Factory.SimpleStringImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleString> {

		/**
		 * Gets instance of compound {@link SimpleString} type.
		 *
		 * @param size
		 * 		the size of the {@link SimpleString}
		 *
		 * @return the newly created compound {@link SimpleString} type
		 */
		public static SimpleString getInstance(final Integer size) {
			return SimpleStringImpl.getInstance(size);
		}

		@Override
		public SimpleString getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SimpleString} type implementation.
		 */
		private static final class SimpleStringImpl extends TypeBaseClass implements SimpleString, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private static final LispType ELEMENT_TYPE = Character.INSTANCE;
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private SimpleStringImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link SimpleString} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleString}
			 */
			private SimpleStringImpl(final Integer size) {
				super("SIMPLE-STRING");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link SimpleString} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleString}
			 *
			 * @return the newly created compound {@link SimpleString} type
			 */
			public static SimpleString getInstance(final Integer size) {
				return new SimpleStringImpl(size);
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

				if (!(obj instanceof SimpleString)) {
					return false;
				}

				final SimpleString simpleString = (SimpleString) obj;
				if (simpleString == INSTANCE) {
					return true;
				}

				if (simpleString instanceof SimpleStringImpl) {
					final SimpleStringImpl simpleStringImpl = (SimpleStringImpl) simpleString;

					return (size == null) || size.equals(simpleStringImpl.size);
				}

				return TypeUtils.isArrayLispTypeEqual(this, simpleString);
			}

			@Override
			public java.lang.String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

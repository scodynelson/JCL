package jcl.types.strings;

import jcl.types.LispType;
import jcl.types.TypeFactory;
import jcl.types.arrays.SimpleArray;
import jcl.types.characters.Character;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import jcl.types.util.TypeUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code SimpleString} is a specialized one-dimensional {@code SimpleArray} whose elements are of type {@code Character}
 * or a subtype of type {@code Character}. When used as a type specifier for object creation, {@code SimpleString} means
 * (simple-array character (size)).
 * <p/>
 * {@code SimpleString} -> {@code String} -> {@code Vector} -> {@code SimpleArray} -> {@code Array} -> {@code Sequence} -> {@code T}
 */
public interface SimpleString extends String, SimpleArray {

	SimpleString INSTANCE = new Factory.SimpleStringImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleString> {

		@Override
		public SimpleString getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code SimpleString} type.
		 *
		 * @param size the size of the {@code SimpleString}
		 * @return the newly created compound {@code SimpleString} type
		 */
		public static SimpleString getInstance(final Integer size) {
			return new SimpleStringImpl(size);
		}

		/**
		 * Inner {@code SimpleString} type implementation.
		 */
		private static class SimpleStringImpl implements SimpleString, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private final LispType elementType = Character.INSTANCE;

			/**
			 * Private constructor.
			 */
			private SimpleStringImpl() {
				size = null;
			}

			/**
			 * Private constructor for compound {@code SimpleString} type.
			 *
			 * @param size the size of the {@code SimpleString}
			 */
			private SimpleStringImpl(final Integer size) {
				this.size = new DimensionsDesignator(size);
			}

			@Override
			public DimensionsDesignator getDimensions() {
				return size;
			}

			@Override
			public LispType getElementType() {
				return elementType;
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof LispType)) {
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
			public int hashCode() {
				return new HashCodeBuilder()
						.append(size)
						.append(elementType)
						.toHashCode();
			}

			@Override
			public java.lang.String toString() {
				return "SimpleStringImpl{" +
						"size=" + size +
						", elementType=" + elementType +
						'}';
			}
		}
	}
}

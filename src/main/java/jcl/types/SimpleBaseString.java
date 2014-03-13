package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.Integer;
import java.lang.String;

/**
 * The type {@code SimpleBaseString} is equivalent to (simple-array base-char (*)).
 * <p/>
 * {@code SimpleBaseString} -> {@code BaseString} -> {@code SimpleString} -> {@code String} -> {@code Vector} -> {@code SimpleArray} -> {@code Array} -> {@code Sequence} -> {@code T}
 */
public interface SimpleBaseString extends BaseString, SimpleString {

	SimpleBaseString INSTANCE = new Factory.SimpleBaseStringImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleBaseString> {

		@Override
		public SimpleBaseString getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code SimpleBaseString} type.
		 *
		 * @param size the size of the {@code SimpleBaseString}
		 * @return the newly created compound {@code SimpleBaseString} type
		 */
		public static SimpleBaseString getInstance(final Integer size) {
			return SimpleBaseStringImpl.getInstance(size);
		}

		/**
		 * Inner {@code SimpleBaseString} type implementation.
		 */
		private static class SimpleBaseStringImpl implements SimpleBaseString, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private final LispType elementType = BaseChar.INSTANCE;

			/**
			 * Private constructor.
			 */
			private SimpleBaseStringImpl() {
				size = null;
			}

			/**
			 * Private constructor for compound {@code SimpleBaseString} type.
			 *
			 * @param size the size of the {@code SimpleBaseString}
			 */
			private SimpleBaseStringImpl(final Integer size) {
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

			/**
			 * Gets instance of compound {@code SimpleBaseString} type.
			 *
			 * @param size the size of the {@code SimpleBaseString}
			 * @return the newly created compound {@code SimpleBaseString} type
			 */
			public static SimpleBaseString getInstance(final Integer size) {
				return new SimpleBaseStringImpl(size);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SimpleBaseString)) {
					return false;
				}

				final SimpleBaseString simpleBaseString = (SimpleBaseString) obj;
				if (simpleBaseString == INSTANCE) {
					return true;
				}

				if (simpleBaseString instanceof SimpleBaseStringImpl) {
					final SimpleBaseStringImpl simpleBaseStringImpl = (SimpleBaseStringImpl) simpleBaseString;

					return (size == null) || size.equals(simpleBaseStringImpl.size);
				}

				return TypeUtils.isArrayLispTypeEqual(this, simpleBaseString);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder()
						.append(size)
						.append(elementType)
						.toHashCode();
			}

			@Override
			public String toString() {
				return "SimpleBaseStringImpl{"
						+ "size=" + size
						+ ", elementType=" + elementType
						+ '}';
			}
		}
	}
}

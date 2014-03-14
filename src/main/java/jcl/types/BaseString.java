package jcl.types;

import jcl.LispType;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.Integer;

/**
 * The type {@code BaseString} is equivalent to (vector base-char). The {@code BaseString} representation is the most
 * efficient {@code String} representation that can hold an arbitrary sequence of {@code StandardCharacter}s.
 * <p/>
 * {@code BaseString} -> {@code String} -> {@code Vector} -> {@code Array} -> {@code Sequence} -> {@code T}
 */
public interface BaseString extends String {

	BaseString INSTANCE = new Factory.BaseStringImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BaseString> {

		@Override
		public BaseString getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code BaseString} type.
		 *
		 * @param size the size of the {@code BaseString}
		 * @return the newly created compound {@code BaseString} type
		 */
		public static BaseString getInstance(final Integer size) {
			return BaseStringImpl.getInstance(size);
		}

		/**
		 * Inner {@code BaseString} type implementation.
		 */
		private static class BaseStringImpl implements BaseString, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private final LispType elementType = BaseChar.INSTANCE;

			/**
			 * Private constructor.
			 */
			private BaseStringImpl() {
				size = null;
			}

			/**
			 * Private constructor for compound {@code BaseString} type.
			 *
			 * @param size the size of the {@code BaseString}
			 */
			private BaseStringImpl(final Integer size) {
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
			 * Gets instance of compound {@code BaseString} type.
			 *
			 * @param size the size of the {@code BaseString}
			 * @return the newly created compound {@code BaseString} type
			 */
			public static BaseString getInstance(final Integer size) {
				return new BaseStringImpl(size);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof BaseString)) {
					return false;
				}

				final BaseString baseString = (BaseString) obj;
				if (baseString == INSTANCE) {
					return true;
				}

				if (baseString instanceof BaseStringImpl) {
					final BaseStringImpl baseStringImpl = (BaseStringImpl) baseString;

					return (size == null) || size.equals(baseStringImpl.size);
				}

				return TypeUtils.isArrayLispTypeEqual(this, baseString);
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
				return "BaseStringImpl{"
						+ "size=" + size
						+ ", elementType=" + elementType
						+ '}';
			}
		}
	}
}

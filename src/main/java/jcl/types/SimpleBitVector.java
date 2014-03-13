package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.Integer;
import java.lang.String;

/**
 * A {@code SimpleBitVector} is a type of a {@code BitVector} that is not displaced to another {@code Array}, has no fill
 * pointer, and is not expressly adjustable is a subtype of type {@code SimpleBitVector}.
 * <p/>
 * {@code SimpleBitVector} -> {@code BitVector} -> {@code Vector} -> {@code SimpleArray} -> {@code Array} -> {@code Sequence} -> {@code T}
 */
public interface SimpleBitVector extends BitVector, SimpleArray {

	SimpleBitVector INSTANCE = new Factory.SimpleBitVectorImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleBitVector> {

		@Override
		public SimpleBitVector getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code SimpleBitVector} type.
		 *
		 * @param size the size of the {@code SimpleBitVector}
		 * @return the newly created compound {@code SimpleBitVector} type
		 */
		public static SimpleBitVector getInstance(final Integer size) {
			return SimpleBitVectorImpl.getInstance(size);
		}

		/**
		 * Inner {@code SimpleBitVector} type implementation.
		 */
		private static class SimpleBitVectorImpl implements SimpleBitVector, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private static final LispType ELEMENT_TYPE = Bit.INSTANCE;

			/**
			 * Private constructor.
			 */
			private SimpleBitVectorImpl() {
				size = null;
			}

			/**
			 * Private constructor for compound {@code SimpleBitVector} type.
			 *
			 * @param size the size of the {@code SimpleBitVector}
			 */
			private SimpleBitVectorImpl(final Integer size) {
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
			 * Gets instance of compound {@code SimpleBitVector} type.
			 *
			 * @param size the size of the {@code SimpleBitVector}
			 * @return the newly created compound {@code SimpleBitVector} type
			 */
			public static SimpleBitVector getInstance(final Integer size) {
				return new SimpleBitVectorImpl(size);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SimpleBitVector)) {
					return false;
				}

				final SimpleBitVector simpleBitVector = (SimpleBitVector) obj;
				if (simpleBitVector == INSTANCE) {
					return true;
				}

				if (simpleBitVector instanceof SimpleBitVectorImpl) {
					final SimpleBitVectorImpl simpleBitVectorImpl = (SimpleBitVectorImpl) simpleBitVector;

					return (size == null) || size.equals(simpleBitVectorImpl.size);
				}

				return TypeUtils.isArrayLispTypeEqual(this, simpleBitVector);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder()
						.append(size)
						.append(ELEMENT_TYPE)
						.toHashCode();
			}

			@Override
			public String toString() {
				return "SimpleBitVectorImpl{"
						+ "size=" + size
						+ ", elementType=" + ELEMENT_TYPE
						+ '}';
			}
		}
	}
}

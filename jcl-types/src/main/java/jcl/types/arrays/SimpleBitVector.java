package jcl.types.arrays;

import jcl.types.LispType;
import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.numbers.Bit;
import jcl.types.sequences.Sequence;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import jcl.types.util.TypeUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code SimpleBitVector} is a type of a {@code BitVector} that is not displaced to another {@code Array}, has no fill
 * pointer, and is not expressly adjustable is a subtype of type {@code SimpleBitVector}.
 */
public interface SimpleBitVector extends BitVector, Vector, SimpleArray, Array, Sequence, T {

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
			return new SimpleBitVectorImpl(size);
		}

		/**
		 * Inner {@code SimpleBitVector} type implementation.
		 */
		private static class SimpleBitVectorImpl implements SimpleBitVector, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private final LispType elementType = Bit.INSTANCE;

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
				return elementType;
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
						.append(elementType)
						.toHashCode();
			}

			@Override
			public String toString() {
				return "SimpleBitVectorImpl{" +
						"size=" + size +
						", elementType=" + elementType +
						'}';
			}
		}
	}
}

package jcl.types.arrays;

import jcl.types.LispType;
import jcl.types.TypeFactory;
import jcl.types.numbers.Bit;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import jcl.types.util.TypeUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code BitVector} is a {@code Vector} the element type of which is {@code Bit}.
 * <p/>
 * The type {@code Vector} is a subtype of type {@code Vector}, for {@code BitVector} means (vector bit).
 * <p/>
 * {@code BitVector} -> {@code Vector} -> {@code Array} -> {@code Sequence} -> {@code T}
 */
public interface BitVector extends Vector {

	BitVector INSTANCE = new Factory.BitVectorImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BitVector> {

		@Override
		public BitVector getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code BitVector} type.
		 *
		 * @param size the size of the {@code BitVector}
		 * @return the newly created compound {@code BitVector} type
		 */
		public static BitVector getInstance(final Integer size) {
			return BitVectorImpl.getInstance(size);
		}

		/**
		 * Inner {@code BitVector} type implementation.
		 */
		private static class BitVectorImpl implements BitVector, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private static final LispType ELEMENT_TYPE = Bit.INSTANCE;

			/**
			 * Private constructor.
			 */
			private BitVectorImpl() {
				size = null;
			}

			/**
			 * Private constructor for compound {@code BitVector} type.
			 *
			 * @param size the size of the {@code BitVector}
			 */
			private BitVectorImpl(final Integer size) {
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
			 * Gets instance of compound {@code BitVector} type.
			 *
			 * @param size the size of the {@code BitVector}
			 * @return the newly created compound {@code BitVector} type
			 */
			public static BitVector getInstance(final Integer size) {
				return new BitVectorImpl(size);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof BitVector)) {
					return false;
				}

				final BitVector bitVector = (BitVector) obj;
				if (bitVector == INSTANCE) {
					return true;
				}

				if (bitVector instanceof BitVectorImpl) {
					final BitVectorImpl bitVectorImpl = (BitVectorImpl) bitVector;

					return (size == null) || size.equals(bitVectorImpl.size);
				}

				return TypeUtils.isArrayLispTypeEqual(this, bitVector);
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
				return "BitVectorImpl{"
						+ "size=" + size
						+ ", elementType=" + ELEMENT_TYPE
						+ '}';
			}
		}
	}
}

package jcl.types;

import jcl.LispType;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.Integer;
import java.lang.String;

/**
 * A {@link BitVector} is a {@link Vector} the element type of which is {@link Bit}.
 * <p>
 * The type {@link Vector} is a subtype of type {@link Vector}, for {@link BitVector} means (vector bit).
 * <p>
 * {@link BitVector} -> {@link Vector} -> {@link Array} -> {@link Sequence} -> {@link T}
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
		 * Gets instance of compound {@link BitVector} type.
		 *
		 * @param size
		 * 		the size of the {@link BitVector}
		 *
		 * @return the newly created compound {@link BitVector} type
		 */
		public static BitVector getInstance(final Integer size) {
			return BitVectorImpl.getInstance(size);
		}

		/**
		 * Inner {@link BitVector} type implementation.
		 */
		private static class BitVectorImpl extends TypeBaseClass implements BitVector, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private static final LispType ELEMENT_TYPE = Bit.INSTANCE;

			/**
			 * Private constructor.
			 */
			private BitVectorImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link BitVector} type.
			 *
			 * @param size
			 * 		the size of the {@link BitVector}
			 */
			private BitVectorImpl(final Integer size) {
				super("BIT-VECTOR", GlobalPackageStruct.COMMON_LISP);
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
			 * Gets instance of compound {@link BitVector} type.
			 *
			 * @param size
			 * 		the size of the {@link BitVector}
			 *
			 * @return the newly created compound {@link BitVector} type
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
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

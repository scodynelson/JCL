/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.lang.Integer;

import jcl.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link BitVector} is a {@link Vector} the element type of which is {@link Bit}.
 * <p>
 * The type {@link Vector} is a subtype of type {@link Vector}, for {@link BitVector} means (vector bit).
 * <p>
 * {@link BitVector} -&gt; {@link Vector} -&gt; {@link Array} -&gt; {@link Sequence} -&gt; {@link T}
 */
public interface BitVector extends Vector {

	/**
	 * Singleton instance of the {@link BitVector} type.
	 */
	BitVector INSTANCE = new Factory.BitVectorImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BitVector> {

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

		@Override
		public BitVector getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BitVector} type implementation.
		 */
		private static final class BitVectorImpl extends TypeBaseClass implements BitVector, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 6131204063063057512L;

			/**
			 * Static {@link Bit} element type for {@link BitVector} types.
			 */
			private static final LispType ELEMENT_TYPE = Bit.INSTANCE;

			/**
			 * The dimensions of the {@link BitVector} type.
			 */
			private final DimensionsDesignator size;

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
				super("BIT-VECTOR");
				this.size = new DimensionsDesignator(size);
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
			public DimensionsDesignator getDimensions() {
				return size;
			}

			@Override
			public LispType getElementType() {
				return ELEMENT_TYPE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .append(size)
				                            .toHashCode();
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
			public java.lang.String toString() {
				final java.util.List<Integer> dimensionsValue = size.getDimensions();
				return '(' + getName() + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue.toString()) + ')';
			}
		}
	}
}

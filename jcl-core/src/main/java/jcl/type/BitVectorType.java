/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.util.List;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;

/**
 * A {@link BitVectorType} is a {@link VectorType} the element type of which is {@link BitType}.
 * <p>
 * The type {@link VectorType} is a subtype of type {@link VectorType}, for {@link BitVectorType} means (vector bit).
 * <p>
 * {@link BitVectorType} -&gt; {@link VectorType} -&gt; {@link ArrayType} -&gt; {@link SequenceType} -&gt; {@link
 * TType}
 */
public interface BitVectorType extends VectorType {

	/**
	 * Singleton instance of the {@link BitVectorType} type.
	 */
	BitVectorType INSTANCE = new Factory.BitVectorTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BitVectorType> {

		/**
		 * Gets instance of compound {@link BitVectorType} type.
		 *
		 * @param size
		 * 		the size of the {@link BitVectorType}
		 *
		 * @return the newly created compound {@link BitVectorType} type
		 */
		public static BitVectorType getInstance(final Integer size) {
			return BitVectorTypeImpl.getInstance(size);
		}

		@Override
		public BitVectorType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BitVectorType} type implementation.
		 */
		private static final class BitVectorTypeImpl extends TypeBaseClass implements BitVectorType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Static {@link BitType} element type for {@link BitVectorType} types.
			 */
			private static final LispType ELEMENT_TYPE = BitType.INSTANCE;

			/**
			 * The dimensions of the {@link BitVectorType} type.
			 */
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private BitVectorTypeImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link BitVectorType} type.
			 *
			 * @param size
			 * 		the size of the {@link BitVectorType}
			 */
			private BitVectorTypeImpl(final Integer size) {
				super("BIT-VECTOR");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link BitVectorType} type.
			 *
			 * @param size
			 * 		the size of the {@link BitVectorType}
			 *
			 * @return the newly created compound {@link BitVectorType} type
			 */
			public static BitVectorType getInstance(final Integer size) {
				return new BitVectorTypeImpl(size);
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
			public boolean typeEquals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof BitVectorType)) {
					return false;
				}

				final BitVectorType bitVectorType = (BitVectorType) obj;
				if (bitVectorType == INSTANCE) {
					return true;
				}

				if (bitVectorType instanceof BitVectorTypeImpl) {
					final BitVectorTypeImpl bitVectorTypeImpl = (BitVectorTypeImpl) bitVectorType;

					return (size == null) || size.equals(bitVectorTypeImpl.size);
				}

				return TypeUtils.isArrayTypeEqual(this, bitVectorType);
			}

			@Override
			public String toString() {
				final List<Integer> dimensionsValue = size.getDimensions();
				return '(' + getName() + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue.toString()) + ')';
			}
		}
	}
}

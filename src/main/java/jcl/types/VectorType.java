/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.util.List;

import jcl.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link VectorType} is any one-dimensional {@link ArrayType}.
 * <p>
 * The type {@link VectorType} is a subtype of type {@link ArrayType}; for all types x, (vector x) is the same as
 * (array x (*)).
 * <p>
 * The type (vector t), the type {@link String}, and the type {@link BitVectorType} are disjoint subtypes of type
 * {@link VectorType}.
 * <p>
 * {@link VectorType} -&gt; {@link ArrayType} -&gt; {@link SequenceType} -&gt; {@link TType}
 */
public interface VectorType extends ArrayType, SequenceType {

	/**
	 * Singleton instance of the {@link VectorType} type.
	 */
	VectorType INSTANCE = new Factory.VectorTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<VectorType> {

		/**
		 * Gets instance of compound {@link VectorType} type.
		 *
		 * @param size
		 * 		the size of the {@link VectorType}
		 * @param elementType
		 * 		the types of elements within the {@link VectorType}
		 *
		 * @return the newly created compound {@link VectorType} type
		 */
		public static VectorType getInstance(final Integer size, final LispType elementType) {
			return VectorTypeImpl.getInstance(size, elementType);
		}

		@Override
		public VectorType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link VectorType} type implementation.
		 */
		private static final class VectorTypeImpl extends TypeBaseClass implements VectorType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The dimensions of the {@link VectorType} type.
			 */
			private final DimensionsDesignator size;

			/**
			 * Element type for {@link VectorType} type.
			 */
			private final LispType elementType;

			/**
			 * Private constructor.
			 */
			private VectorTypeImpl() {
				this(null, null);
			}

			/**
			 * Private constructor for compound {@link VectorType} type.
			 *
			 * @param size
			 * 		the size of the {@link VectorType}
			 * @param elementType
			 * 		the types of elements within the {@link VectorType}
			 */
			private VectorTypeImpl(final Integer size, final LispType elementType) {
				super("VECTOR");
				this.size = new DimensionsDesignator(size);
				this.elementType = elementType;
			}

			/**
			 * Gets instance of compound {@link VectorType} type.
			 *
			 * @param size
			 * 		the size of the {@link VectorType}
			 * @param elementType
			 * 		the types of elements within the {@link VectorType}
			 *
			 * @return the newly created compound {@link VectorType} type
			 */
			public static VectorType getInstance(final Integer size, final LispType elementType) {
				return new VectorTypeImpl(size, elementType);
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
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .append(size)
				                            .append(elementType)
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof VectorType)) {
					return false;
				}

				final VectorType vectorType = (VectorType) obj;
				if (vectorType == INSTANCE) {
					return true;
				}

				if (vectorType instanceof VectorTypeImpl) {
					return checkVectorTypeImplEquality((VectorTypeImpl) vectorType);
				}

				return TypeUtils.isArrayTypeEqual(this, vectorType);
			}

			/**
			 * This method checks the equality of the provided {@code vectorTypeImpl} object to this instance.
			 *
			 * @param vectorTypeImpl
			 * 		the VectorTypeImpl object to test for equality
			 *
			 * @return true if the {@code vectorTypeImpl} object is equivalent to this instance; false otherwise
			 */
			private boolean checkVectorTypeImplEquality(final VectorTypeImpl vectorTypeImpl) {
				if (size == null) {
					return elementType.equals(vectorTypeImpl.elementType);
				}

				return size.equals(vectorTypeImpl.size) && elementType.equals(vectorTypeImpl.elementType);
			}

			@Override
			public String toString() {
				final List<Integer> dimensionsValue = size.getDimensions();
				return '(' + getName() + ' ' + elementType + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue) + ')';
			}
		}
	}
}

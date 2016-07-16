/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.util.List;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link SimpleVectorType} is a type of a {@link VectorType} that is not displaced to another {@link ArrayType}, has
 * no fill pointer, is not expressly adjustable, and is able to hold elements of any type is a subtype of type {@link
 * SimpleVectorType}.
 * <p>
 * The type {@link SimpleVectorType} is a subtype of type {@link VectorType}, and is a subtype of type (vector t).
 * <p>
 * {@link SimpleVectorType} -&gt; {@link VectorType} -&gt; {@link SimpleArrayType} -&gt; {@link ArrayType} -&gt; {@link
 * SequenceType} -&gt; {@link TType}
 */
public interface SimpleVectorType extends VectorType, SimpleArrayType {

	/**
	 * Singleton instance of the {@link SimpleVectorType} type.
	 */
	SimpleVectorType INSTANCE = new Factory.SimpleVectorTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleVectorType> {

		/**
		 * Gets instance of compound {@link SimpleVectorType} type.
		 *
		 * @param size
		 * 		the size of the {@link SimpleVectorType}
		 *
		 * @return the newly created compound {@link SimpleVectorType} type
		 */
		public static SimpleVectorType getInstance(final Integer size) {
			return SimpleVectorTypeImpl.getInstance(size, TType.INSTANCE);
		}

		/**
		 * Gets instance of compound {@link SimpleVectorType} type.
		 *
		 * @param size
		 * 		the size of the {@link SimpleVectorType}
		 * @param elementType
		 * 		the types of elements within the {@link SimpleVectorType}
		 *
		 * @return the newly created compound {@link SimpleVectorType} type
		 */
		public static SimpleVectorType getInstance(final Integer size, final LispType elementType) {
			return SimpleVectorTypeImpl.getInstance(size, elementType);
		}

		@Override
		public SimpleVectorType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SimpleVectorType} type implementation.
		 */
		private static final class SimpleVectorTypeImpl extends TypeBaseClass implements SimpleVectorType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The dimensions of the {@link SimpleVectorType} type.
			 */
			private final DimensionsDesignator size;

			/**
			 * Element type for {@link SimpleVectorType} type.
			 */
			private final LispType elementType;

			/**
			 * Private constructor.
			 */
			private SimpleVectorTypeImpl() {
				this(null, null);
			}

			/**
			 * Private constructor for compound {@link SimpleVectorType} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleVectorType}
			 * @param elementType
			 * 		the types of elements within the {@link SimpleVectorType}
			 */
			private SimpleVectorTypeImpl(final Integer size, final LispType elementType) {
				super("SIMPLE-VECTOR");
				this.size = new DimensionsDesignator(size);
				this.elementType = elementType;
			}

			/**
			 * Gets instance of compound {@link SimpleVectorType} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleVectorType}
			 * @param elementType
			 * 		the types of elements within the {@link SimpleVectorType}
			 *
			 * @return the newly created compound {@link SimpleVectorType} type
			 */
			public static SimpleVectorType getInstance(final Integer size, final LispType elementType) {
				return new SimpleVectorTypeImpl(size, elementType);
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

				if (!(obj instanceof SimpleVectorType)) {
					return false;
				}

				final SimpleVectorType simpleVectorType = (SimpleVectorType) obj;
				if (simpleVectorType == INSTANCE) {
					return true;
				}

				if (simpleVectorType instanceof SimpleVectorTypeImpl) {
					return checkSimpleVectorTypeImplEquality((SimpleVectorTypeImpl) simpleVectorType);
				}

				return TypeUtils.isArrayTypeEqual(this, simpleVectorType);
			}

			/**
			 * This method checks the equality of the provided {@code simpleVectorTypeImpl} object to this instance.
			 *
			 * @param simpleVectorTypeImpl
			 * 		the SimpleVectorTypeImpl object to test for equality
			 *
			 * @return true if the {@code simpleVectorTypeImpl} object is equivalent to this instance; false otherwise
			 */
			private boolean checkSimpleVectorTypeImplEquality(final SimpleVectorTypeImpl simpleVectorTypeImpl) {
				if (size == null) {
					return elementType.equals(simpleVectorTypeImpl.elementType);
				}

				return size.equals(simpleVectorTypeImpl.size) && elementType.equals(simpleVectorTypeImpl.elementType);
			}

			@Override
			public String toString() {
				final List<Integer> dimensionsValue = size.getDimensions();
				return '(' + getName() + ' ' + elementType + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue.toString()) + ')';
			}
		}
	}
}

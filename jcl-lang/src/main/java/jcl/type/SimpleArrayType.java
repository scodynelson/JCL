/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.util.List;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link SimpleArrayType} is the type of an {@link ArrayType} that is not displaced to another {@link ArrayType},
 * has no fill pointer, and is not expressly adjustable.
 * <p>
 * The types {@link SimpleVectorType}, {@link SimpleStringType}, and {@link SimpleBitVectorType} are disjoint subtypes
 * of type {@link SimpleArrayType}, for they respectively mean (simple-array t (*)), the union of all (simple-array c
 * (*)) for any c being a subtype of type {@link CharacterType}, and (simple-array bit (*)).
 * <p>
 * {@link SimpleArrayType} -&gt; {@link ArrayType} -&gt; {@link TType}
 */
public interface SimpleArrayType extends ArrayType {

	/**
	 * Singleton instance of the {@link SimpleArrayType} type.
	 */
	SimpleArrayType INSTANCE = new Factory.SimpleArrayTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleArrayType> {

		/**
		 * Gets instance of compound {@link SimpleArrayType} type.
		 *
		 * @param dimensions
		 * 		the dimensions of the {@link SimpleArrayType}
		 * @param elementType
		 * 		the types of elements within the {@link SimpleArrayType}
		 *
		 * @return the newly created compound {@link SimpleArrayType} type
		 */
		public static SimpleArrayType getInstance(final List<Integer> dimensions, final LispType elementType) {
			return SimpleArrayTypeImpl.getInstance(dimensions, elementType);
		}

		@Override
		public SimpleArrayType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SimpleArrayType} type implementation.
		 */
		private static final class SimpleArrayTypeImpl extends TypeBaseClass implements SimpleArrayType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The dimensions of the {@link SimpleArrayType} type.
			 */
			private final DimensionsDesignator dimensions;

			/**
			 * Element type for {@link SimpleArrayType} type.
			 */
			private final LispType elementType;

			/**
			 * Private constructor.
			 */
			private SimpleArrayTypeImpl() {
				this(null, null);
			}

			/**
			 * Private constructor for compound {@link SimpleArrayType} type.
			 *
			 * @param dimensions
			 * 		the dimensions of the {@link SimpleArrayType}
			 * @param elementType
			 * 		the types of elements within the {@link SimpleArrayType}
			 */
			private SimpleArrayTypeImpl(final List<Integer> dimensions, final LispType elementType) {
				super("SIMPLE-ARRAY");
				this.dimensions = new DimensionsDesignator(dimensions);
				this.elementType = elementType;
			}

			/**
			 * Gets instance of compound {@link SimpleArrayType} type.
			 *
			 * @param dimensions
			 * 		the dimensions of the {@link SimpleArrayType}
			 * @param elementType
			 * 		the types of elements within the {@link SimpleArrayType}
			 *
			 * @return the newly created compound {@link SimpleArrayType} type
			 */
			public static SimpleArrayType getInstance(final List<Integer> dimensions, final LispType elementType) {
				return new SimpleArrayTypeImpl(dimensions, elementType);
			}

			@Override
			public DimensionsDesignator getDimensions() {
				return dimensions;
			}

			@Override
			public LispType getElementType() {
				return elementType;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .append(dimensions)
				                            .append(elementType)
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SimpleArrayType)) {
					return false;
				}

				final SimpleArrayType simpleArrayType = (SimpleArrayType) obj;
				if (simpleArrayType == INSTANCE) {
					return true;
				}

				if (simpleArrayType instanceof SimpleArrayTypeImpl) {
					return checkSimpleArrayTypeImplEquality((SimpleArrayTypeImpl) simpleArrayType);
				}

				return TypeUtils.isArrayTypeEqual(this, simpleArrayType);
			}

			/**
			 * This method checks the equality of the provided {@code simpleArrayTypeImpl} object to this instance.
			 *
			 * @param simpleArrayTypeImpl
			 * 		the SimpleArrayTypeImpl object to test for equality
			 *
			 * @return true if the {@code simpleArrayTypeImpl} object is equivalent to this instance; false otherwise
			 */
			private boolean checkSimpleArrayTypeImplEquality(final SimpleArrayTypeImpl simpleArrayTypeImpl) {
				if (dimensions == null) {
					return elementType.equals(simpleArrayTypeImpl.elementType);
				}

				return dimensions.equals(simpleArrayTypeImpl.dimensions) && elementType.equals(simpleArrayTypeImpl.elementType);
			}

			@Override
			public String toString() {
				final List<Integer> dimensionsValue = dimensions.getDimensions();
				return '(' + getName() + ' ' + elementType + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue.toString()) + ')';
			}
		}
	}
}

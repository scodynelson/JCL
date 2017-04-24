/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.util.List;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;

/**
 * An {@link ArrayType} contains objects arranged according to a Cartesian coordinate system. An {@link ArrayType}
 * provides mappings from a set of fixnums {i0,i1,...,ir-1} to corresponding elements of the {@link ArrayType}, where 0
 * &lt;=ij &lt; dj, r is the rank of the {@link ArrayType}, and dj is the size of dimension j of the {@link ArrayType}.
 * <p>
 * When an {@link ArrayType} is created, the program requesting its creation may declare that all elements are of a
 * particular type, called the expressed array element type.
 * <p>
 * {@link ArrayType} -&gt; {@link TType}
 */
public interface ArrayType extends TType {

	/**
	 * Singleton instance of the {@link ArrayType} type.
	 */
	ArrayType INSTANCE = new Factory.ArrayTypeImpl();

	/**
	 * This method returns the dimensions of the Array type.
	 *
	 * @return the dimensions of the Array type
	 */
	DimensionsDesignator getDimensions();

	/**
	 * This method returns the element type of the Array type.
	 *
	 * @return the element type of the Array type
	 */
	LispType getElementType();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ArrayType> {

		/**
		 * Gets instance of compound {@link ArrayType} type.
		 *
		 * @param dimensions
		 * 		the dimensions of the {@link ArrayType}
		 * @param elementType
		 * 		the types of elements within the {@link ArrayType}
		 *
		 * @return the newly created compound {@link ArrayType} type
		 */
		public static ArrayType getInstance(final List<Integer> dimensions, final LispType elementType) {
			return ArrayTypeImpl.getInstance(dimensions, elementType);
		}

		@Override
		public ArrayType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ArrayType} type implementation.
		 */
		private static final class ArrayTypeImpl extends TypeBaseClass implements ArrayType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The dimensions of the {@link ArrayType} type.
			 */
			private final DimensionsDesignator dimensions;

			/**
			 * Element type for {@link ArrayType} type.
			 */
			private final LispType elementType;

			/**
			 * Private constructor.
			 */
			private ArrayTypeImpl() {
				this(null, null);
			}

			/**
			 * Private constructor for compound {@link ArrayType} type.
			 *
			 * @param dimensions
			 * 		the dimensions of the {@link ArrayType}
			 * @param elementType
			 * 		the types of elements within the {@link ArrayType}
			 */
			private ArrayTypeImpl(final List<Integer> dimensions, final LispType elementType) {
				super("ARRAY");
				this.dimensions = new DimensionsDesignator(dimensions);
				this.elementType = elementType;
			}

			/**
			 * Gets instance of compound {@link ArrayType} type.
			 *
			 * @param dimensions
			 * 		the dimensions of the {@link ArrayType}
			 * @param elementType
			 * 		the types of elements within the {@link ArrayType}
			 *
			 * @return the newly created compound {@link ArrayType} type
			 */
			public static ArrayType getInstance(final List<Integer> dimensions, final LispType elementType) {
				return new ArrayTypeImpl(dimensions, elementType);
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
			public boolean typeEquals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof ArrayType)) {
					return false;
				}

				final ArrayType arrayType = (ArrayType) obj;
				if (arrayType == INSTANCE) {
					return true;
				}

				if (arrayType instanceof ArrayTypeImpl) {
					return checkArrayImplEquality((ArrayTypeImpl) arrayType);
				}

				return TypeUtils.isArrayTypeEqual(this, arrayType);
			}

			/**
			 * This method checks the equality of the provide {@code arrayTypeImpl} object to this instance.
			 *
			 * @param arrayTypeImpl
			 * 		the ArrayTypeImpl object to test for equality
			 *
			 * @return true if the {@code arrayTypeImpl} object is equivalent to this instance; false otherwise
			 */
			private boolean checkArrayImplEquality(final ArrayTypeImpl arrayTypeImpl) {
				if (dimensions == null) {
					return elementType.isOfType(arrayTypeImpl.elementType);
				}

				return dimensions.equals(arrayTypeImpl.dimensions) && elementType.isOfType(arrayTypeImpl.elementType);
			}

			@Override
			public String toString() {
				final List<Integer> dimensionsValue = dimensions.getDimensions();
				return '(' + getName() + ' ' + elementType + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue) + ')';
			}
		}
	}
}

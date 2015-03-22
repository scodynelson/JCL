/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.lang.Integer;
import java.lang.String;
import java.util.List;

import jcl.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * An {@link Array} contains objects arranged according to a Cartesian coordinate system. An {@link Array} provides
 * mappings from a set of fixnums {i0,i1,...,ir-1} to corresponding elements of the {@link Array}, where 0 &lt;=ij &lt;
 * dj, r is the rank of the {@link Array}, and dj is the size of dimension j of the {@link Array}.
 * <p>
 * When an {@link Array} is created, the program requesting its creation may declare that all elements are of a
 * particular type, called the expressed array element type.
 * <p>
 * {@link Array} -&gt; {@link T}
 */
public interface Array extends T {

	/**
	 * Singleton instance of the {@link Array} type.
	 */
	Array INSTANCE = new Factory.ArrayImpl();

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
	class Factory implements TypeFactory<Array> {

		/**
		 * Gets instance of compound {@link Array} type.
		 *
		 * @param dimensions
		 * 		the dimensions of the {@link Array}
		 * @param elementType
		 * 		the types of elements within the {@link Array}
		 *
		 * @return the newly created compound {@link Array} type
		 */
		public static Array getInstance(final List<Integer> dimensions, final LispType elementType) {
			return ArrayImpl.getInstance(dimensions, elementType);
		}

		@Override
		public Array getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Array} type implementation.
		 */
		private static final class ArrayImpl extends TypeBaseClass implements Array, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 2325035154860979758L;

			/**
			 * The dimensions of the {@link Array} type.
			 */
			private final DimensionsDesignator dimensions;

			/**
			 * Element type for {@link Array} type.
			 */
			private final LispType elementType;

			/**
			 * Private constructor.
			 */
			private ArrayImpl() {
				this(null, null);
			}

			/**
			 * Private constructor for compound {@link Array} type.
			 *
			 * @param dimensions
			 * 		the dimensions of the {@link Array}
			 * @param elementType
			 * 		the types of elements within the {@link Array}
			 */
			private ArrayImpl(final List<Integer> dimensions, final LispType elementType) {
				super("ARRAY");
				this.dimensions = new DimensionsDesignator(dimensions);
				this.elementType = elementType;
			}

			/**
			 * Gets instance of compound {@link Array} type.
			 *
			 * @param dimensions
			 * 		the dimensions of the {@link Array}
			 * @param elementType
			 * 		the types of elements within the {@link Array}
			 *
			 * @return the newly created compound {@link Array} type
			 */
			public static Array getInstance(final List<Integer> dimensions, final LispType elementType) {
				return new ArrayImpl(dimensions, elementType);
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

				if (!(obj instanceof Array)) {
					return false;
				}

				final Array array = (Array) obj;
				if (array == INSTANCE) {
					return true;
				}

				if (array instanceof ArrayImpl) {
					return checkArrayImplEquality((ArrayImpl) array);
				}

				return TypeUtils.isArrayLispTypeEqual(this, array);
			}

			/**
			 * This method checks the equality of the provide {@code arrayImpl} object to this instance.
			 *
			 * @param arrayImpl
			 * 		the ArrayImpl object to test for equality
			 *
			 * @return true if the {@code arrayImpl} object is equivalent to this instance; false otherwise
			 */
			private boolean checkArrayImplEquality(final ArrayImpl arrayImpl) {
				if (dimensions == null) {
					return elementType.equals(arrayImpl.elementType);
				}

				return dimensions.equals(arrayImpl.dimensions) && elementType.equals(arrayImpl.elementType);
			}

			@Override
			public String toString() {
				final List<Integer> dimensionsValue = dimensions.getDimensions();
				return '(' + getName() + ' ' + elementType + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue) + ')';
			}
		}
	}
}

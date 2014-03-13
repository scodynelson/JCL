package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.Integer;
import java.lang.String;
import java.util.List;

/**
 * An {@code Array} contains objects arranged according to a Cartesian coordinate system. An {@code Array} provides mappings
 * from a set of fixnums {i0,i1,...,ir-1} to corresponding elements of the {@code Array}, where 0 <=ij < dj, r is the rank
 * of the {@code Array}, and dj is the size of dimension j of the {@code Array}.
 * <p/>
 * When an {@code Array} is created, the program requesting its creation may declare that all elements are of a particular
 * type, called the expressed array element type.
 * <p/>
 * {@code Array} -> {@code T}
 */
public interface Array extends T {

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

		@Override
		public Array getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code Array} type.
		 *
		 * @param dimensions  the dimensions of the {@code Array}
		 * @param elementType the types of elements within the {@code Array}
		 * @return the newly created compound {@code Array} type
		 */
		public static Array getInstance(final List<Integer> dimensions, final LispType elementType) {
			return ArrayImpl.getInstance(dimensions, elementType);
		}

		/**
		 * Inner {@code Array} type implementation.
		 */
		private static class ArrayImpl implements Array, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator dimensions;
			private final LispType elementType;

			/**
			 * Private constructor.
			 */
			private ArrayImpl() {
				dimensions = null;
				elementType = null;
			}

			/**
			 * Private constructor for compound {@code Array} type.
			 *
			 * @param dimensions  the dimensions of the {@code Array}
			 * @param elementType the types of elements within the {@code Array}
			 */
			private ArrayImpl(final List<Integer> dimensions, final LispType elementType) {
				this.dimensions = new DimensionsDesignator(dimensions);
				this.elementType = elementType;
			}

			@Override
			public DimensionsDesignator getDimensions() {
				return dimensions;
			}

			@Override
			public LispType getElementType() {
				return elementType;
			}

			/**
			 * Gets instance of compound {@code Array} type.
			 *
			 * @param dimensions  the dimensions of the {@code Array}
			 * @param elementType the types of elements within the {@code Array}
			 * @return the newly created compound {@code Array} type
			 */
			public static Array getInstance(final List<Integer> dimensions, final LispType elementType) {
				return new ArrayImpl(dimensions, elementType);
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
					final ArrayImpl arrayImpl = (ArrayImpl) array;

					if (dimensions == null) {
						return elementType.equals(arrayImpl.elementType);
					}

					return dimensions.equals(arrayImpl.dimensions) && elementType.equals(arrayImpl.elementType);
				}

				return TypeUtils.isArrayLispTypeEqual(this, array);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder()
						.append(dimensions)
						.append(elementType)
						.toHashCode();
			}

			@Override
			public String toString() {
				return "ArrayImpl{"
						+ "dimensions=" + dimensions
						+ ", elementType=" + elementType
						+ '}';
			}
		}
	}
}

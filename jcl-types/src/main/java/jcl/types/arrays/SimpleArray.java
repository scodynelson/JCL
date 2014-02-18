package jcl.types.arrays;

import jcl.types.LispType;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import jcl.types.util.TypeUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.List;

/**
 * A {@code SimpleArray} is the type of an {@code Array} that is not displaced to another {@code Array}, has no fill pointer,
 * and is not expressly adjustable.
 * <p/>
 * The types {@code SimpleVector}, {@code SimpleString}, and {@code SimpleBitVector} are disjoint subtypes of type
 * {@code SimpleArray}, for they respectively mean (simple-array t (*)), the union of all (simple-array c (*)) for any
 * c being a subtype of type {@code Character}, and (simple-array bit (*)).
 * <p/>
 * {@code SimpleArray} -> {@code Array} -> {@code T}
 */
public interface SimpleArray extends Array {

	SimpleArray INSTANCE = new Factory.SimpleArrayImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleArray> {

		@Override
		public SimpleArray getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code SimpleArray} type.
		 *
		 * @param dimensions  the dimensions of the {@code SimpleArray}
		 * @param elementType the types of elements within the {@code SimpleArray}
		 * @return the newly created compound {@code SimpleArray} type
		 */
		public static SimpleArray getInstance(final List<Integer> dimensions, final LispType elementType) {
			return new SimpleArrayImpl(dimensions, elementType);
		}

		/**
		 * Inner {@code SimpleArray} type implementation.
		 */
		private static class SimpleArrayImpl implements SimpleArray, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator dimensions;
			private final LispType elementType;

			/**
			 * Private constructor.
			 */
			private SimpleArrayImpl() {
				dimensions = null;
				elementType = null;
			}

			/**
			 * Private constructor for compound {@code SimpleArray} type.
			 *
			 * @param dimensions  the dimensions of the {@code SimpleArray}
			 * @param elementType the types of elements within the {@code SimpleArray}
			 */
			private SimpleArrayImpl(final List<Integer> dimensions, final LispType elementType) {
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

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SimpleArray)) {
					return false;
				}

				final SimpleArray simpleArray = (SimpleArray) obj;
				if (simpleArray == INSTANCE) {
					return true;
				}

				if (simpleArray instanceof SimpleArrayImpl) {
					final SimpleArrayImpl simpleArrayImpl = (SimpleArrayImpl) simpleArray;

					if (dimensions == null) {
						return elementType.equals(simpleArrayImpl.elementType);
					}

					return dimensions.equals(simpleArrayImpl.dimensions) && elementType.equals(simpleArrayImpl.elementType);
				}

				return TypeUtils.isArrayLispTypeEqual(this, simpleArray);
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
				return "SimpleArrayImpl{" +
						"dimensions=" + dimensions +
						", elementType=" + elementType +
						'}';
			}
		}
	}
}

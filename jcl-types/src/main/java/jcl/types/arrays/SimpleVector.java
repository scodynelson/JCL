package jcl.types.arrays;

import jcl.types.LispType;
import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import jcl.types.util.TypeUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code SimpleVector} is a type of a {@code Vector} that is not displaced to another {@code Array}, has no fill pointer,
 * is not expressly adjustable, and is able to hold elements of any type is a subtype of type {@code SimpleVector}.
 * <p/>
 * The type {@code SimpleVector} is a subtype of type {@code Vector}, and is a subtype of type (vector t).
 * <p/>
 * {@code SimpleVector} -> {@code Vector} -> {@code SimpleArray} -> {@code Array} -> {@code Sequence} -> {@code T}
 */
public interface SimpleVector extends Vector, SimpleArray {

	SimpleVector INSTANCE = new Factory.SimpleVectorImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleVector> {

		@Override
		public SimpleVector getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code SimpleVector} type.
		 *
		 * @param size the size of the {@code SimpleVector}
		 * @return the newly created compound {@code SimpleVector} type
		 */
		public static SimpleVector getInstance(final Integer size) {
			return new SimpleVectorImpl(size);
		}

		/**
		 * Inner {@code SimpleVector} type implementation.
		 */
		private static class SimpleVectorImpl implements SimpleVector, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private final LispType elementType = T.INSTANCE;

			/**
			 * Private constructor.
			 */
			private SimpleVectorImpl() {
				size = null;
			}

			/**
			 * Private constructor for compound {@code SimpleVector} type.
			 *
			 * @param size the size of the {@code SimpleVector}
			 */
			private SimpleVectorImpl(final Integer size) {
				this.size = new DimensionsDesignator(size);
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
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SimpleVector)) {
					return false;
				}

				final SimpleVector simpleVector = (SimpleVector) obj;
				if (simpleVector == INSTANCE) {
					return true;
				}

				if (simpleVector instanceof SimpleVectorImpl) {
					final SimpleVectorImpl simpleVectorImpl = (SimpleVectorImpl) simpleVector;

					if (size == null) {
						return elementType.equals(simpleVectorImpl.elementType);
					}

					return size.equals(simpleVectorImpl.size) && elementType.equals(simpleVectorImpl.elementType);
				}

				return TypeUtils.isArrayLispTypeEqual(this, simpleVector);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder()
						.append(size)
						.append(elementType)
						.toHashCode();
			}

			@Override
			public String toString() {
				return "SimpleVectorImpl{" +
						"size=" + size +
						", elementType=" + elementType +
						'}';
			}
		}
	}
}

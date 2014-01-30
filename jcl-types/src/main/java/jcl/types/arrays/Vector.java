package jcl.types.arrays;

import jcl.types.LispType;
import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.sequences.Sequence;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import jcl.types.util.TypeUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Vector} is any one-dimensional {@code Array}.
 * <p/>
 * The type {@code Vector} is a subtype of type {@code Array}; for all types x, (vector x) is the same as (array x (*)).
 * <p/>
 * The type (vector t), the type {@code String}, and the type {@code BitVector} are disjoint subtypes of type
 * {@code Vector}.
 */
public interface Vector extends Array, Sequence, T {

	Vector INSTANCE = new Factory.VectorImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Vector> {

		@Override
		public Vector getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code Vector} type.
		 *
		 * @param size        the size of the {@code Vector}
		 * @param elementType the types of elements within the {@code Vector}
		 * @return the newly created compound {@code Vector} type
		 */
		public static Vector getInstance(final Integer size, final LispType elementType) {
			return new VectorImpl(size, elementType);
		}

		/**
		 * Inner {@code Vector} type implementation.
		 */
		private static class VectorImpl implements Vector, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final DimensionsDesignator size;
			private final LispType elementType;

			/**
			 * Private constructor.
			 */
			private VectorImpl() {
				size = null;
				elementType = null;
			}

			/**
			 * Private constructor for compound {@code Vector} type.
			 *
			 * @param size        the size of the {@code Vector}
			 * @param elementType the types of elements within the {@code Vector}
			 */
			private VectorImpl(final Integer size, final LispType elementType) {
				this.size = new DimensionsDesignator(size);
				this.elementType = elementType;
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

				if (!(obj instanceof Vector)) {
					return false;
				}

				final Vector vector = (Vector) obj;
				if (vector == INSTANCE) {
					return true;
				}

				if (vector instanceof VectorImpl) {
					final VectorImpl vectorImpl = (VectorImpl) vector;

					if (size == null) {
						return elementType.equals(vectorImpl.elementType);
					}

					return size.equals(vectorImpl.size) && elementType.equals(vectorImpl.elementType);
				}

				return TypeUtils.isArrayLispTypeEqual(this, vector);
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
				return "VectorImpl{" +
						"size=" + size +
						", elementType=" + elementType +
						'}';
			}
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.Integer;
import java.lang.String;

/**
 * A {@link Vector} is any one-dimensional {@link Array}.
 * <p>
 * The type {@link Vector} is a subtype of type {@link Array}; for all types x, (vector x) is the same as
 * (array x (*)).
 * <p>
 * The type (vector t), the type {@link String}, and the type {@link BitVector} are disjoint subtypes of type
 * {@link Vector}.
 * <p>
 * {@link Vector} -&gt; {@link Array} -&gt; {@link Sequence} -&gt; {@link T}
 */
public interface Vector extends Array, Sequence {

	/**
	 * Singleton instance of the {@link Vector} type.
	 */
	Vector INSTANCE = new Factory.VectorImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Vector> {

		/**
		 * Gets instance of compound {@link Vector} type.
		 *
		 * @param size
		 * 		the size of the {@link Vector}
		 * @param elementType
		 * 		the types of elements within the {@link Vector}
		 *
		 * @return the newly created compound {@link Vector} type
		 */
		public static Vector getInstance(final Integer size, final LispType elementType) {
			return VectorImpl.getInstance(size, elementType);
		}

		@Override
		public Vector getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Vector} type implementation.
		 */
		private static final class VectorImpl extends TypeBaseClass implements Vector, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 8666531696390635781L;

			/**
			 * The dimensions of the {@link Vector} type.
			 */
			private final DimensionsDesignator size;

			/**
			 * Element type for {@link Vector} type.
			 */
			private final LispType elementType;

			/**
			 * Private constructor.
			 */
			private VectorImpl() {
				this(null, null);
			}

			/**
			 * Private constructor for compound {@link Vector} type.
			 *
			 * @param size
			 * 		the size of the {@link Vector}
			 * @param elementType
			 * 		the types of elements within the {@link Vector}
			 */
			private VectorImpl(final Integer size, final LispType elementType) {
				super("VECTOR");
				this.size = new DimensionsDesignator(size);
				this.elementType = elementType;
			}

			/**
			 * Gets instance of compound {@link Vector} type.
			 *
			 * @param size
			 * 		the size of the {@link Vector}
			 * @param elementType
			 * 		the types of elements within the {@link Vector}
			 *
			 * @return the newly created compound {@link Vector} type
			 */
			public static Vector getInstance(final Integer size, final LispType elementType) {
				return new VectorImpl(size, elementType);
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
				return HashCodeBuilder.reflectionHashCode(this);
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
					return checkVectorImplEquality((VectorImpl) vector);
				}

				return TypeUtils.isArrayLispTypeEqual(this, vector);
			}

			/**
			 * This method checks the equality of the provide {@code vectorImpl} object to this instance.
			 *
			 * @param vectorImpl
			 * 		the VectorImpl object to test for equality
			 *
			 * @return true if the {@code vectorImpl} object is equivalent to this instance; false otherwise
			 */
			private boolean checkVectorImplEquality(final VectorImpl vectorImpl) {
				if (size == null) {
					return elementType.equals(vectorImpl.elementType);
				}

				return size.equals(vectorImpl.size) && elementType.equals(vectorImpl.elementType);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}

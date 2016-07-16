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
 * A {@link SimpleBitVectorType} is a type of a {@link BitVectorType} that is not displaced to another {@link
 * ArrayType}, has no fill pointer, and is not expressly adjustable is a subtype of type {@link SimpleBitVectorType}.
 * <p>
 * {@link SimpleBitVectorType} -&gt; {@link BitVectorType} -&gt; {@link VectorType} -&gt; {@link SimpleArrayType} -&gt;
 * {@link ArrayType} -&gt; {@link SequenceType} -&gt; {@link TType}
 */
public interface SimpleBitVectorType extends BitVectorType, SimpleArrayType {

	/**
	 * Singleton instance of the {@link SimpleBitVectorType} type.
	 */
	SimpleBitVectorType INSTANCE = new Factory.SimpleBitVectorTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleBitVectorType> {

		/**
		 * Gets instance of compound {@link SimpleBitVectorType} type.
		 *
		 * @param size
		 * 		the size of the {@link SimpleBitVectorType}
		 *
		 * @return the newly created compound {@link SimpleBitVectorType} type
		 */
		public static SimpleBitVectorType getInstance(final Integer size) {
			return SimpleBitVectorTypeImpl.getInstance(size);
		}

		@Override
		public SimpleBitVectorType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SimpleBitVectorType} type implementation.
		 */
		private static final class SimpleBitVectorTypeImpl extends TypeBaseClass implements SimpleBitVectorType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Static {@link BitType} element type for {@link SimpleBitVectorType} types.
			 */
			private static final LispType ELEMENT_TYPE = BitType.INSTANCE;

			/**
			 * The dimensions of the {@link SimpleBitVectorType} type.
			 */
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private SimpleBitVectorTypeImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link SimpleBitVectorType} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleBitVectorType}
			 */
			private SimpleBitVectorTypeImpl(final Integer size) {
				super("SIMPLE-BIT-VECTOR");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link SimpleBitVectorType} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleBitVectorType}
			 *
			 * @return the newly created compound {@link SimpleBitVectorType} type
			 */
			public static SimpleBitVectorType getInstance(final Integer size) {
				return new SimpleBitVectorTypeImpl(size);
			}

			@Override
			public DimensionsDesignator getDimensions() {
				return size;
			}

			@Override
			public LispType getElementType() {
				return ELEMENT_TYPE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .append(size)
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SimpleBitVectorType)) {
					return false;
				}

				final SimpleBitVectorType simpleBitVectorType = (SimpleBitVectorType) obj;
				if (simpleBitVectorType == INSTANCE) {
					return true;
				}

				if (simpleBitVectorType instanceof SimpleBitVectorTypeImpl) {
					final SimpleBitVectorTypeImpl simpleBitVectorTypeImpl = (SimpleBitVectorTypeImpl) simpleBitVectorType;

					return (size == null) || size.equals(simpleBitVectorTypeImpl.size);
				}

				return TypeUtils.isArrayTypeEqual(this, simpleBitVectorType);
			}

			@Override
			public String toString() {
				final List<Integer> dimensionsValue = size.getDimensions();
				return '(' + getName() + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue.toString()) + ')';
			}
		}
	}
}

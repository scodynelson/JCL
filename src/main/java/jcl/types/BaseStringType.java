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
 * The type {@link BaseStringType} is equivalent to (vector base-char). The {@link BaseStringType} representation is
 * the most efficient {@link StringType} representation that can hold an arbitrary sequence of {@link
 * StandardCharType}s.
 * <p>
 * {@link BaseStringType} -&gt; {@link StringType} -&gt; {@link VectorType} -&gt; {@link ArrayType} -&gt; {@link
 * SequenceType} -&gt; {@link TType}
 */
public interface BaseStringType extends StringType {

	/**
	 * Singleton instance of the {@link BaseStringType} type.
	 */
	BaseStringType INSTANCE = new Factory.BaseStringTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BaseStringType> {

		/**
		 * Gets instance of compound {@link BaseStringType} type.
		 *
		 * @param size
		 * 		the size of the {@link BaseStringType}
		 *
		 * @return the newly created compound {@link BaseStringType} type
		 */
		public static BaseStringType getInstance(final Integer size) {
			return BaseStringTypeImpl.getInstance(size);
		}

		@Override
		public BaseStringType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BaseStringType} type implementation.
		 */
		private static final class BaseStringTypeImpl extends TypeBaseClass implements BaseStringType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Static {@link BaseCharType} element type for {@link BaseStringType} types.
			 */
			private static final LispType ELEMENT_TYPE = BaseCharType.INSTANCE;

			/**
			 * The dimensions of the {@link BaseStringType} type.
			 */
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private BaseStringTypeImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link BaseStringType} type.
			 *
			 * @param size
			 * 		the size of the {@link BaseStringType}
			 */
			private BaseStringTypeImpl(final Integer size) {
				super("BASE-STRING");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link BaseStringType} type.
			 *
			 * @param size
			 * 		the size of the {@link BaseStringType}
			 *
			 * @return the newly created compound {@link BaseStringType} type
			 */
			public static BaseStringType getInstance(final Integer size) {
				return new BaseStringTypeImpl(size);
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

				if (!(obj instanceof BaseStringType)) {
					return false;
				}

				final BaseStringType baseStringType = (BaseStringType) obj;
				if (baseStringType == INSTANCE) {
					return true;
				}

				if (baseStringType instanceof BaseStringTypeImpl) {
					final BaseStringTypeImpl baseStringTypeImpl = (BaseStringTypeImpl) baseStringType;

					return (size == null) || size.equals(baseStringTypeImpl.size);
				}

				return TypeUtils.isArrayTypeEqual(this, baseStringType);
			}

			@Override
			public String toString() {
				final List<Integer> dimensionsValue = size.getDimensions();
				return '(' + getName() + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue.toString()) + ')';
			}
		}
	}
}

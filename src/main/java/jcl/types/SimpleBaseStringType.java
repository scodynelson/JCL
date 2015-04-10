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
 * The type {@link SimpleBaseStringType} is equivalent to (simple-array base-char (*)).
 * <p>
 * {@link SimpleBaseStringType} -&gt; {@link BaseStringType} -&gt; {@link SimpleStringType} -&gt; {@link String}
 * -&gt; {@link VectorType} -&gt; {@link SimpleArrayType} -&gt; {@link ArrayType} -&gt; {@link SequenceType} -&gt;
 * {@link TType}
 */
public interface SimpleBaseStringType extends BaseStringType, SimpleStringType {

	/**
	 * Singleton instance of the {@link SimpleBaseStringType} type.
	 */
	SimpleBaseStringType INSTANCE = new Factory.SimpleBaseStringTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleBaseStringType> {

		/**
		 * Gets instance of compound {@link SimpleBaseStringType} type.
		 *
		 * @param size
		 * 		the size of the {@link SimpleBaseStringType}
		 *
		 * @return the newly created compound {@link SimpleBaseStringType} type
		 */
		public static SimpleBaseStringType getInstance(final Integer size) {
			return SimpleBaseStringTypeImpl.getInstance(size);
		}

		@Override
		public SimpleBaseStringType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SimpleBaseStringType} type implementation.
		 */
		private static final class SimpleBaseStringTypeImpl extends TypeBaseClass implements SimpleBaseStringType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 5384574210137691247L;

			/**
			 * Static {@link BaseCharType} element type for {@link SimpleBaseStringType} types.
			 */
			private static final LispType ELEMENT_TYPE = BaseCharType.INSTANCE;

			/**
			 * The dimensions of the {@link SimpleBaseStringType} type.
			 */
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private SimpleBaseStringTypeImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link SimpleBaseStringType} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleBaseStringType}
			 */
			private SimpleBaseStringTypeImpl(final Integer size) {
				super("SIMPLE-BASE-STRING");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link SimpleBaseStringType} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleBaseStringType}
			 *
			 * @return the newly created compound {@link SimpleBaseStringType} type
			 */
			public static SimpleBaseStringType getInstance(final Integer size) {
				return new SimpleBaseStringTypeImpl(size);
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

				if (!(obj instanceof SimpleBaseStringType)) {
					return false;
				}

				final SimpleBaseStringType simpleBaseStringType = (SimpleBaseStringType) obj;
				if (simpleBaseStringType == INSTANCE) {
					return true;
				}

				if (simpleBaseStringType instanceof SimpleBaseStringTypeImpl) {
					final SimpleBaseStringTypeImpl simpleBaseStringTypeImpl = (SimpleBaseStringTypeImpl) simpleBaseStringType;

					return (size == null) || size.equals(simpleBaseStringTypeImpl.size);
				}

				return TypeUtils.isArrayTypeEqual(this, simpleBaseStringType);
			}

			@Override
			public String toString() {
				final List<Integer> dimensionsValue = size.getDimensions();
				return '(' + getName() + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue.toString()) + ')';
			}
		}
	}
}

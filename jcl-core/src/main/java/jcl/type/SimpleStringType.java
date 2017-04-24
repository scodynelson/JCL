/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.util.List;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;

/**
 * A {@link SimpleStringType} is a specialized one-dimensional {@link SimpleArrayType} whose elements are of type
 * {@link CharacterType} or a subtype of type {@link CharacterType}. When used as a type specifier for object creation,
 * {@link SimpleStringType} means (simple-array character (size)).
 * <p>
 * {@link SimpleStringType} -&gt; {@link StringType} -&gt; {@link VectorType} -&gt; {@link SimpleArrayType} -&gt;
 * {@link ArrayType} -&gt; {@link SequenceType} -&gt; {@link TType}
 */
public interface SimpleStringType extends StringType, SimpleArrayType {

	/**
	 * Singleton instance of the {@link SimpleStringType} type.
	 */
	SimpleStringType INSTANCE = new Factory.SimpleStringTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleStringType> {

		/**
		 * Gets instance of compound {@link SimpleStringType} type.
		 *
		 * @param size
		 * 		the size of the {@link SimpleStringType}
		 *
		 * @return the newly created compound {@link SimpleStringType} type
		 */
		public static SimpleStringType getInstance(final Integer size) {
			return SimpleStringTypeImpl.getInstance(size);
		}

		@Override
		public SimpleStringType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SimpleStringType} type implementation.
		 */
		private static final class SimpleStringTypeImpl extends TypeBaseClass implements SimpleStringType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Static {@link CharacterType} element type for {@link SimpleStringType} types.
			 */
			private static final LispType ELEMENT_TYPE = CharacterType.INSTANCE;

			/**
			 * The dimensions of the {@link SimpleStringType} type.
			 */
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private SimpleStringTypeImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link SimpleStringType} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleStringType}
			 */
			private SimpleStringTypeImpl(final Integer size) {
				super("SIMPLE-STRING");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link SimpleStringType} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleStringType}
			 *
			 * @return the newly created compound {@link SimpleStringType} type
			 */
			public static SimpleStringType getInstance(final Integer size) {
				return new SimpleStringTypeImpl(size);
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
			public boolean typeEquals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SimpleStringType)) {
					return false;
				}

				final SimpleStringType simpleStringType = (SimpleStringType) obj;
				if (simpleStringType == INSTANCE) {
					return true;
				}

				if (simpleStringType instanceof SimpleStringTypeImpl) {
					final SimpleStringTypeImpl simpleStringTypeImpl = (SimpleStringTypeImpl) simpleStringType;

					return (size == null) || size.equals(simpleStringTypeImpl.size);
				}

				return TypeUtils.isArrayTypeEqual(this, simpleStringType);
			}

			@Override
			public String toString() {
				final List<Integer> dimensionsValue = size.getDimensions();
				return '(' + getName() + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue.toString()) + ')';
			}
		}
	}
}

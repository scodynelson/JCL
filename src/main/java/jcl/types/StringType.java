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
 * A {@link StringType} is a specialized {@link VectorType} whose elements are of type {@link CharacterType} or a
 * subtype of type {@link CharacterType}. When used as a type specifier for object creation, {@link StringType} means
 * (vector character).
 * <p>
 * {@link StringType} -&gt; {@link VectorType} -&gt; {@link ArrayType} -&gt; {@link SequenceType} -&gt; {@link TType}
 */
public interface StringType extends VectorType {

	/**
	 * Singleton instance of the {@link StringType} type.
	 */
	StringType INSTANCE = new Factory.StringTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StringType> {

		/**
		 * Gets instance of compound {@link StringType} type.
		 *
		 * @param size
		 * 		the size of the {@link StringType}
		 *
		 * @return the newly created compound {@link StringType} type
		 */
		public static StringType getInstance(final Integer size) {
			return StringTypeImpl.getInstance(size);
		}

		@Override
		public StringType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StringType} type implementation.
		 */
		private static final class StringTypeImpl extends TypeBaseClass implements StringType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -8137934390331662477L;

			/**
			 * Static {@link CharacterType} element type for {@link StringType} types.
			 */
			private static final LispType ELEMENT_TYPE = CharacterType.INSTANCE;

			/**
			 * The dimensions of the {@link StringType} type.
			 */
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private StringTypeImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link StringType} type.
			 *
			 * @param size
			 * 		the size of the {@link StringType}
			 */
			private StringTypeImpl(final Integer size) {
				super("STRING");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link StringType} type.
			 *
			 * @param size
			 * 		the size of the {@link StringType}
			 *
			 * @return the newly created compound {@link StringType} type
			 */
			public static StringType getInstance(final Integer size) {
				return new StringTypeImpl(size);
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

				if (!(obj instanceof StringType)) {
					return false;
				}

				final StringType stringType = (StringType) obj;
				if (stringType == INSTANCE) {
					return true;
				}

				if (stringType instanceof StringTypeImpl) {
					final StringTypeImpl stringTypeImpl = (StringTypeImpl) stringType;

					return (size == null) || size.equals(stringTypeImpl.size);
				}

				return TypeUtils.isArrayTypeEqual(this, stringType);
			}

			@Override
			public String toString() {
				final List<Integer> dimensionsValue = size.getDimensions();
				return '(' + getName() + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue.toString()) + ')';
			}
		}
	}
}

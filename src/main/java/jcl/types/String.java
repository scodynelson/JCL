/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.lang.Integer;

import jcl.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link String} is a specialized {@link Vector} whose elements are of type {@link Character} or a subtype of type
 * {@link Character}. When used as a type specifier for object creation, {@link String} means (vector character).
 * <p>
 * {@link String} -&gt; {@link Vector} -&gt; {@link Array} -&gt; {@link Sequence} -&gt; {@link T}
 */
public interface String extends Vector {

	/**
	 * Singleton instance of the {@link String} type.
	 */
	String INSTANCE = new Factory.StringImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<String> {

		/**
		 * Gets instance of compound {@link String} type.
		 *
		 * @param size
		 * 		the size of the {@link String}
		 *
		 * @return the newly created compound {@link String} type
		 */
		public static String getInstance(final Integer size) {
			return StringImpl.getInstance(size);
		}

		@Override
		public String getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link String} type implementation.
		 */
		private static final class StringImpl extends TypeBaseClass implements String, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -8137934390331662477L;

			/**
			 * Static {@link Character} element type for {@link String} types.
			 */
			private static final LispType ELEMENT_TYPE = Character.INSTANCE;

			/**
			 * The dimensions of the {@link String} type.
			 */
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private StringImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link String} type.
			 *
			 * @param size
			 * 		the size of the {@link String}
			 */
			private StringImpl(final Integer size) {
				super("STRING");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link String} type.
			 *
			 * @param size
			 * 		the size of the {@link String}
			 *
			 * @return the newly created compound {@link String} type
			 */
			public static String getInstance(final Integer size) {
				return new StringImpl(size);
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

				if (!(obj instanceof String)) {
					return false;
				}

				final String aString = (String) obj;
				if (aString == INSTANCE) {
					return true;
				}

				if (aString instanceof StringImpl) {
					final StringImpl aStringImpl = (StringImpl) aString;

					return (size == null) || size.equals(aStringImpl.size);
				}

				return TypeUtils.isArrayLispTypeEqual(this, aString);
			}

			@Override
			public java.lang.String toString() {
				final java.util.List<Integer> dimensionsValue = size.getDimensions();
				return '(' + getName() + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue.toString()) + ')';
			}
		}
	}
}

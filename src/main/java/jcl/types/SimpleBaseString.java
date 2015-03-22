/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.lang.Integer;
import java.lang.String;

import jcl.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@link SimpleBaseString} is equivalent to (simple-array base-char (*)).
 * <p>
 * {@link SimpleBaseString} -&gt; {@link BaseString} -&gt; {@link SimpleString} -&gt; {@link String}
 * -&gt; {@link Vector} -&gt; {@link SimpleArray} -&gt; {@link Array} -&gt; {@link Sequence} -&gt; {@link T}
 */
public interface SimpleBaseString extends BaseString, SimpleString {

	/**
	 * Singleton instance of the {@link SimpleBaseString} type.
	 */
	SimpleBaseString INSTANCE = new Factory.SimpleBaseStringImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SimpleBaseString> {

		/**
		 * Gets instance of compound {@link SimpleBaseString} type.
		 *
		 * @param size
		 * 		the size of the {@link SimpleBaseString}
		 *
		 * @return the newly created compound {@link SimpleBaseString} type
		 */
		public static SimpleBaseString getInstance(final Integer size) {
			return SimpleBaseStringImpl.getInstance(size);
		}

		@Override
		public SimpleBaseString getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SimpleBaseString} type implementation.
		 */
		private static final class SimpleBaseStringImpl extends TypeBaseClass implements SimpleBaseString, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 5384574210137691247L;

			/**
			 * Static {@link BaseChar} element type for {@link SimpleBaseString} types.
			 */
			private static final LispType ELEMENT_TYPE = BaseChar.INSTANCE;

			/**
			 * The dimensions of the {@link SimpleBaseString} type.
			 */
			private final DimensionsDesignator size;

			/**
			 * Private constructor.
			 */
			private SimpleBaseStringImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link SimpleBaseString} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleBaseString}
			 */
			private SimpleBaseStringImpl(final Integer size) {
				super("SIMPLE-BASE-STRING");
				this.size = new DimensionsDesignator(size);
			}

			/**
			 * Gets instance of compound {@link SimpleBaseString} type.
			 *
			 * @param size
			 * 		the size of the {@link SimpleBaseString}
			 *
			 * @return the newly created compound {@link SimpleBaseString} type
			 */
			public static SimpleBaseString getInstance(final Integer size) {
				return new SimpleBaseStringImpl(size);
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

				if (!(obj instanceof SimpleBaseString)) {
					return false;
				}

				final SimpleBaseString simpleBaseString = (SimpleBaseString) obj;
				if (simpleBaseString == INSTANCE) {
					return true;
				}

				if (simpleBaseString instanceof SimpleBaseStringImpl) {
					final SimpleBaseStringImpl simpleBaseStringImpl = (SimpleBaseStringImpl) simpleBaseString;

					return (size == null) || size.equals(simpleBaseStringImpl.size);
				}

				return TypeUtils.isArrayLispTypeEqual(this, simpleBaseString);
			}

			@Override
			public java.lang.String toString() {
				final java.util.List<Integer> dimensionsValue = size.getDimensions();
				return '(' + getName() + ' ' + ((dimensionsValue == null) ? '*' : dimensionsValue.toString()) + ')';
			}
		}
	}
}

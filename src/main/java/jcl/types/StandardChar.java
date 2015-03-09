/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * The type {@link StandardChar} is fixed set of 96 characters.
 * <p>
 * Any character that is not simple is not a standard character.
 * <p>
 * {@link StandardChar} -> {@link BaseChar} -> {@link Character} -> {@link T}
 */
public interface StandardChar extends BaseChar {

	/**
	 * Singleton instance of the {@link StandardChar} type.
	 */
	StandardChar INSTANCE = new Factory.StandardCharImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardChar> {

		@Override
		public StandardChar getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardChar} type implementation.
		 */
		private static final class StandardCharImpl extends TypeBaseClass implements StandardChar, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 1814050119904469853L;

			/**
			 * Private constructor.
			 */
			private StandardCharImpl() {
				super("STANDARD-CHAR");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardChar);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}

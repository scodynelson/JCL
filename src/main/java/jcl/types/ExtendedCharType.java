/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@link ExtendedCharType} is equivalent to the type (and character (not base-char)).
 * <p>
 * {@link ExtendedCharType} -&gt; {@link CharacterType} -&gt; {@link TType}
 */
public interface ExtendedCharType extends CharacterType {

	/**
	 * Singleton instance of the {@link ExtendedCharType} type.
	 */
	ExtendedCharType INSTANCE = new Factory.ExtendedCharTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ExtendedCharType> {

		@Override
		public ExtendedCharType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ExtendedCharType} type implementation.
		 */
		private static final class ExtendedCharTypeImpl extends TypeBaseClass implements ExtendedCharType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -5215210339292683845L;

			/**
			 * Private constructor.
			 */
			private ExtendedCharTypeImpl() {
				super("EXTENDED-CHAR");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof ExtendedCharType);
			}
		}
	}
}
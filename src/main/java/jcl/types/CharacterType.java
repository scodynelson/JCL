/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link CharacterType} is an object that represents a unitary token in an aggregate quantity of text.
 * <p>
 * The types {@link BaseCharType} and {@link ExtendedCharType} form an exhaustive partition of the type {@link
 * CharacterType}.
 * <p>
 * {@link CharacterType} -&gt; {@link TType}
 */
public interface CharacterType extends TType {

	/**
	 * Singleton instance of the {@link CharacterType} type.
	 */
	CharacterType INSTANCE = new Factory.CharacterTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<CharacterType> {

		@Override
		public CharacterType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link CharacterType} type implementation.
		 */
		private static final class CharacterTypeImpl extends TypeBaseClass implements CharacterType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 8069574443377375137L;

			/**
			 * Private constructor.
			 */
			private CharacterTypeImpl() {
				super("CHARACTER");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof CharacterType);
			}
		}
	}
}

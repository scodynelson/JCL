/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

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
			 * Private constructor.
			 */
			private CharacterTypeImpl() {
				super("CHARACTER");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof CharacterType);
			}
		}
	}
}

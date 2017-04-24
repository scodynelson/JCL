/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * The type {@link BaseCharType} is defined as the upgraded array element type of {@link StandardCharType}.
 * <p>
 * Base characters are distinguished in the following respects:
 * 1. The type {@link StandardCharType} is a subrepertoire of the type {@link BaseCharType}.
 * 2. The selection of base characters that are not standard characters is implementation defined.
 * 3. Only objects of the type {@link BaseCharType} can be elements of a base string.
 * 4. No upper bound is specified for the number of characters in the {@link BaseCharType} repertoire. The lower bound
 * is 96, the number of standard characters.
 * <p>
 * The type {@link StandardCharType} is a subtype of type {@link BaseCharType}.
 * <p>
 * {@link BaseCharType} -&gt; {@link CharacterType} -&gt; {@link TType}
 */
public interface BaseCharType extends CharacterType {

	/**
	 * Singleton instance of the {@link BaseCharType} type.
	 */
	BaseCharType INSTANCE = new Factory.BaseCharTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BaseCharType> {

		@Override
		public BaseCharType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BaseCharType} type implementation.
		 */
		private static final class BaseCharTypeImpl extends TypeBaseClass implements BaseCharType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BaseCharTypeImpl() {
				super("BASE-CHAR");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof BaseCharType);
			}
		}
	}
}

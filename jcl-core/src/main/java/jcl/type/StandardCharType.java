/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * The type {@link StandardCharType} is fixed set of 96 characters.
 * <p>
 * Any character that is not simple is not a standard character.
 * <p>
 * {@link StandardCharType} -&gt; {@link BaseCharType} -&gt; {@link CharacterType} -&gt; {@link TType}
 */
public interface StandardCharType extends BaseCharType {

	/**
	 * Singleton instance of the {@link StandardCharType} type.
	 */
	StandardCharType INSTANCE = new Factory.StandardCharTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardCharType> {

		@Override
		public StandardCharType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardCharType} type implementation.
		 */
		private static final class StandardCharTypeImpl extends TypeBaseClass implements StandardCharType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StandardCharTypeImpl() {
				super("STANDARD-CHAR");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof StandardCharType);
			}
		}
	}
}

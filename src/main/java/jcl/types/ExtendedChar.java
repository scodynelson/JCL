/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * The type {@link ExtendedChar} is equivalent to the type (and character (not base-char)).
 * <p>
 * {@link ExtendedChar} -&gt; {@link Character} -&gt; {@link T}
 */
public interface ExtendedChar extends Character {

	/**
	 * Singleton instance of the {@link ExtendedChar} type.
	 */
	ExtendedChar INSTANCE = new Factory.ExtendedCharImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ExtendedChar> {

		@Override
		public ExtendedChar getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ExtendedChar} type implementation.
		 */
		private static final class ExtendedCharImpl extends TypeBaseClass implements ExtendedChar, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -5215210339292683845L;

			/**
			 * Private constructor.
			 */
			private ExtendedCharImpl() {
				super("EXTENDED-CHAR");
			}
		}
	}
}

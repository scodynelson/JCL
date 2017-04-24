/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

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
			 * Private constructor.
			 */
			private ExtendedCharTypeImpl() {
				super("EXTENDED-CHAR");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof ExtendedCharType);
			}
		}
	}
}

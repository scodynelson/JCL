/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * The type {@link TType} is the set of all objects. It is a supertype of every type, including itself. Every object is
 * of type {@link TType}.
 * <p>
 * {@link TType}
 */
public interface TType extends LispType {

	/**
	 * Singleton instance of the {@link TType} type.
	 */
	TType INSTANCE = new Factory.TTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<TType> {

		@Override
		public TType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link TType} type implementation.
		 */
		private static final class TTypeImpl extends TypeBaseClass implements TType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private TTypeImpl() {
				super("T");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof TType);
			}
		}
	}
}

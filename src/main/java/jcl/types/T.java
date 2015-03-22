/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * The type {@link T} is the set of all objects. It is a supertype of every type, including itself. Every object is of
 * type {@link T}.
 * <p>
 * {@link T}
 */
public interface T extends LispType {

	/**
	 * Singleton instance of the {@link T} type.
	 */
	T INSTANCE = new Factory.TImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<T> {

		@Override
		public T getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link T} type implementation.
		 */
		private static final class TImpl extends TypeBaseClass implements T, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -5539110546772298423L;

			/**
			 * Private constructor.
			 */
			private TImpl() {
				super("T");
			}
		}
	}
}

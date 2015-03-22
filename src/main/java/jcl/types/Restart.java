/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * A {@link Restart} represents a {@link Function} that can be called to perform some form of recovery action, usually
 * a transfer of control to an outer point in the running program.
 * <p>
 * A {@link Restart} has only dynamic extent relative to the scope of the binding form which establishes it.
 * <p>
 * {@link Restart} -&gt; {@link T}
 */
public interface Restart extends T {

	/**
	 * Singleton instance of the {@link Restart} type.
	 */
	Restart INSTANCE = new Factory.RestartImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Restart> {

		@Override
		public Restart getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Restart} type implementation.
		 */
		private static final class RestartImpl extends TypeBaseClass implements Restart, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -7905323000782513518L;

			/**
			 * Private constructor.
			 */
			private RestartImpl() {
				super("RESTART");
			}
		}
	}
}

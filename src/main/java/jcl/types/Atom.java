/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.NotTypeSpecifier;

/**
 * An {@link Atom} is a type equivalent to (not cons).
 * <p>
 * {@link Atom} -&gt; {@link T}
 */
public interface Atom extends T {

	/**
	 * Singleton instance of the {@link Atom} type.
	 */
	Atom INSTANCE = new Factory.AtomImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Atom> {

		@Override
		public Atom getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Atom} type implementation.
		 */
		private static final class AtomImpl extends NotTypeSpecifier implements Atom, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 4480283950568664048L;

			/**
			 * Private constructor.
			 */
			private AtomImpl() {
				super("ATOM", Cons.INSTANCE);
			}

			@Override
			public java.lang.String toString() {
				return getName();
			}
		}
	}
}

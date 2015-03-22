/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * A {@link Pathname} is a structured object which represents a filename.
 * <p>
 * There are two kinds of {@link Pathname}s---physical pathnames and {@link LogicalPathname}s.
 * <p>
 * {@link Pathname} -&gt; {@link T}
 */
public interface Pathname extends T {

	/**
	 * Singleton instance of the {@link Pathname} type.
	 */
	Pathname INSTANCE = new Factory.PathnameImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Pathname> {

		@Override
		public Pathname getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Pathname} type implementation.
		 */
		private static final class PathnameImpl extends TypeBaseClass implements Pathname, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -7343108902657436359L;

			/**
			 * Private constructor.
			 */
			private PathnameImpl() {
				super("PATHNAME");
			}
		}
	}
}

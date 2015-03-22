/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * A {@link Package} is a namespace that maps symbol names to {@link Symbol}s.
 * <p>
 * {@link Package} -&gt; {@link T}
 */
public interface Package extends T {

	/**
	 * Singleton instance of the {@link Package} type.
	 */
	Package INSTANCE = new Factory.PackageImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Package> {

		@Override
		public Package getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Package} type implementation.
		 */
		private static final class PackageImpl extends TypeBaseClass implements Package, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -3289071503998628075L;

			/**
			 * Private constructor.
			 */
			private PackageImpl() {
				super("PACKAGE");
			}
		}
	}
}

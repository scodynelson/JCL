/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * A {@link StandardMethod} is the default method type {@link Class}.
 * <p>
 * {@link StandardMethod} -&gt; {@link Method} -&gt; {@link StandardObject} -&gt; {@link T}
 */
public interface StandardMethod extends Method, StandardObject {

	/**
	 * Singleton instance of the {@link StandardMethod} type.
	 */
	StandardMethod INSTANCE = new Factory.StandardMethodImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardMethod> {

		@Override
		public StandardMethod getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardMethod} type implementation.
		 */
		private static final class StandardMethodImpl extends TypeBaseClass implements StandardMethod, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 604577373535063426L;

			/**
			 * Private constructor.
			 */
			private StandardMethodImpl() {
				super("STANDARD-METHOD");
			}
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * A {@link StandardClass} is the default class type {@link Class}.
 * <p>
 * {@link StandardClass} -&gt; {@link Class} -&gt; {@link StandardObject} -&gt; {@link T}
 */
public interface StandardClass extends Class {

	/**
	 * Singleton instance of the {@link StandardClass} type.
	 */
	StandardClass INSTANCE = new Factory.StandardClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardClass> {

		@Override
		public StandardClass getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardClass} type implementation.
		 */
		private static final class StandardClassImpl extends TypeBaseClass implements StandardClass, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 8869486055336833896L;

			/**
			 * Private constructor.
			 */
			private StandardClassImpl() {
				super("STANDARD-CLASS");
			}
		}
	}
}

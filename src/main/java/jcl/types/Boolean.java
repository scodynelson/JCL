/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.OrTypeSpecifier;

/**
 * The type {@link Boolean} contains the {@link Symbol}s {@link T} and {@link NIL}, which represent true and false,
 * respectively.
 * <p>
 * {@link Boolean} -&gt; {@link Symbol} -&gt; {@link T}
 */
public interface Boolean extends Symbol {

	/**
	 * Singleton instance of the {@link Boolean} type.
	 */
	Boolean INSTANCE = new Factory.BooleanImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Boolean> {

		@Override
		public Boolean getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Boolean} type implementation.
		 */
		private static final class BooleanImpl extends OrTypeSpecifier implements Boolean, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -6424016818134588642L;

			/**
			 * Private constructor.
			 */
			private BooleanImpl() {
				super("BOOLEAN", T.INSTANCE, NIL.INSTANCE);
			}

			@Override
			public java.lang.String toString() {
				return getName();
			}
		}
	}
}

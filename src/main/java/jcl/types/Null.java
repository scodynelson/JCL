/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * The only object of type {@link Null} is {@link NIL}, which represents the empty {@link List} and can also be notated
 * ().
 * <p>
 * {@link Null} -&gt; {@link Symbol} -&gt; {@link List} -&gt; {@link Sequence} -&gt; {@link T}
 */
public interface Null extends Symbol, List {

	/**
	 * Singleton instance of the {@link Null} type.
	 */
	Null INSTANCE = new Factory.NullImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Null> {

		@Override
		public Null getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Null} type implementation.
		 */
		private static final class NullImpl extends TypeBaseClass implements Null, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -9181095754133674561L;

			/**
			 * Private constructor.
			 */
			private NullImpl() {
				super("NULL");
			}
		}
	}
}

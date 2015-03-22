/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.lang.*;

import jcl.types.typespecifiers.AndTypeSpecifier;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.NotTypeSpecifier;

/**
 * The type {@link Bignum} is defined to be exactly (and integer (not fixnum)).
 * <p>
 * {@link Bignum} -&gt; {@link Integer} -&gt; {@link Rational} -&gt; {@link Real} -&gt; {@link Number} -&gt; {@link T}
 */
public interface Bignum extends Integer {

	/**
	 * Singleton instance of the {@link Bignum} type.
	 */
	Bignum INSTANCE = new Factory.BignumImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Bignum> {

		@Override
		public Bignum getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Bignum} type implementation.
		 */
		private static final class BignumImpl extends AndTypeSpecifier implements Bignum, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -2641268480714921916L;

			/**
			 * Private constructor.
			 */
			private BignumImpl() {
				super("BIGNUM", Integer.INSTANCE, new NotTypeSpecifier(Fixnum.INSTANCE));
			}

			@Override
			public java.lang.String toString() {
				return getName();
			}
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.math.BigInteger;

import jcl.types.typespecifiers.AndTypeSpecifier;
import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * A {@link Fixnum} is an {@link Integer} whose value is between most-negative-fixnum and most-positive-fixnum
 * inclusive. The type {@link Fixnum} is a supertype of (signed-byte 16).
 * <p>
 * {@link Fixnum} -&gt; {@link Integer} -&gt; {@link Rational} -&gt; {@link Real} -&gt; {@link Number} -&gt; {@link T}
 */
public interface Fixnum extends Integer {

	/**
	 * Singleton instance of the {@link Fixnum} type.
	 */
	Fixnum INSTANCE = new Factory.FixnumImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Fixnum> {

		@Override
		public Fixnum getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Fixnum} type implementation.
		 */
		private static final class FixnumImpl extends AndTypeSpecifier implements Fixnum, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 5001042603403390586L;

			/**
			 * Private constructor.
			 */
			private FixnumImpl() {
				super("FIXNUM", getSuper());
			}

			/**
			 * This method gets the {@link Integer} supertype for creating a {@link Fixnum}.
			 *
			 * @return the {@link Integer} supertype
			 */
			private static Integer getSuper() {
				return Integer.Factory.getInstance(BigInteger.valueOf(java.lang.Integer.MIN_VALUE), BigInteger.valueOf(java.lang.Integer.MAX_VALUE));
			}

			@Override
			public java.lang.String toString() {
				return getName();
			}
		}
	}
}

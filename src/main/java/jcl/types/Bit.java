/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.lang.*;
import java.math.BigInteger;

import jcl.types.typespecifiers.AndTypeSpecifier;
import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * The type {@link Bit} is equivalent to the type (integer 0 1) and (unsigned-byte 1).
 * <p>
 * {@link Bit} -&gt; {@link UnsignedByte} -&gt; {@link SignedByte} -&gt; {@link Integer} -&gt; {@link Rational}
 * -&gt; {@link Real} -&gt; {@link Number} -&gt; {@link T}
 */
public interface Bit extends UnsignedByte {

	/**
	 * Singleton instance of the {@link Bit} type.
	 */
	Bit INSTANCE = new Factory.BitImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Bit> {

		@Override
		public Bit getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Bit} type implementation.
		 */
		private static final class BitImpl extends AndTypeSpecifier implements Bit, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 1872465815435985391L;

			/**
			 * Private constructor.
			 */
			private BitImpl() {
				super("BIT", UnsignedByte.Factory.getInstance(BigInteger.ONE));
			}

			@Override
			public java.lang.String toString() {
				return getName();
			}
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.math.BigInteger;

import jcl.types.typespecifiers.AndTypeSpecifier;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.math3.util.ArithmeticUtils;

/**
 * The atomic type specifier {@link UnsignedByte} denotes the same type as is denoted by the type specifier
 * (integer 0 *).
 * <p>
 * This denotes the set of non-negative {@link Integer}s that can be represented in a byte of size s (bits). This is
 * equivalent to (mod m) for m=2^s, or to (integer 0 n) for n=2^s-1. The type {@link UnsignedByte} or the type
 * (unsigned-byte *) is the same as the type (integer 0 *), the set of non-negative {@link Integer}s.
 * <p>
 * {@link UnsignedByte} -&gt; {@link SignedByte} -&gt; {@link Integer} -&gt; {@link Rational} -&gt; {@link Real} -&gt;
 * {@link Number} -&gt; {@link T}
 */
public interface UnsignedByte extends SignedByte {

	/**
	 * Singleton instance of the {@link UnsignedByte} type.
	 */
	UnsignedByte INSTANCE = new Factory.UnsignedByteImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<UnsignedByte> {

		/**
		 * Gets instance of compound {@link UnsignedByte} type.
		 *
		 * @param bits
		 * 		the bits that comprise the byte value
		 *
		 * @return the newly created compound {@link UnsignedByte} type
		 */
		public static UnsignedByte getInstance(final BigInteger bits) {
			return UnsignedByteImpl.getInstance(bits);
		}

		@Override
		public UnsignedByte getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link UnsignedByte} type implementation.
		 */
		private static final class UnsignedByteImpl extends AndTypeSpecifier implements UnsignedByte, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 7776477100685781384L;

			/**
			 * Private constructor.
			 */
			private UnsignedByteImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link UnsignedByte} type.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 */
			private UnsignedByteImpl(final BigInteger bits) {
				super("UNSIGNED-BYTE", getSuper(bits));
			}

			/**
			 * This method gets the {@link Integer} supertype for creating an {@link UnsignedByte}.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 *
			 * @return the {@link Integer} supertype
			 */
			private static Integer getSuper(final BigInteger bits) {
				if (bits != null) {
					final BigInteger upper = ArithmeticUtils.pow(BigInteger.valueOf(2L), bits.subtract(BigInteger.ONE));
					return Integer.Factory.getInstance(BigInteger.ZERO, upper);
				}
				return Integer.Factory.getInstance(BigInteger.ZERO, null);
			}

			/**
			 * Gets instance of compound {@link UnsignedByte} type.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 *
			 * @return the newly created compound {@link UnsignedByte} type
			 */
			public static UnsignedByte getInstance(final BigInteger bits) {
				return new UnsignedByteImpl(bits);
			}

			@Override
			public java.lang.String toString() {
				return getName();
			}
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.math.BigInteger;

import jcl.types.typespecifiers.AndTypeSpecifier;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.util.ArithmeticUtils;

/**
 * The atomic type specifier {@link SignedByte} denotes the same type as is denoted by the type specifier {@link
 * Integer}; however, the list forms of these two type specifiers have different semantics.
 * <p>
 * This denotes the set of {@link Integer}s that can be represented in two's-complement form in a byte of s bits. This
 * is equivalent to (integer -2^s-1 2^s-1-1). The type {@link SignedByte} or the type (signed-byte *) is the same as
 * the type {@link Integer}.
 * <p>
 * {@link SignedByte} -&gt; {@link Integer} -&gt; {@link Rational} -&gt; {@link Real} -&gt; {@link Number}
 * -&gt; {@link T}
 */
public interface SignedByte extends Integer {

	/**
	 * Singleton instance of the {@link SignedByte} type.
	 */
	SignedByte INSTANCE = new Factory.SignedByteImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SignedByte> {

		/**
		 * Gets instance of compound {@link SignedByte} type.
		 *
		 * @param bits
		 * 		the bits that comprise the byte value
		 *
		 * @return the newly created compound {@link SignedByte} type
		 */
		public static SignedByte getInstance(final BigInteger bits) {
			return SignedByteImpl.getInstance(bits);
		}

		@Override
		public SignedByte getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SignedByte} type implementation.
		 */
		private static final class SignedByteImpl extends AndTypeSpecifier implements SignedByte, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 5435575518396163160L;

			/**
			 * Private constructor.
			 */
			private SignedByteImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link SignedByte} type.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 */
			private SignedByteImpl(final BigInteger bits) {
				super("SIGNED-BYTE", getSuper(bits));
			}

			/**
			 * This method gets the {@link Integer} supertype for creating a {@link SignedByte}.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 *
			 * @return the {@link Integer} supertype
			 */
			private static Integer getSuper(final BigInteger bits) {
				if (bits != null) {
					final BigInteger lower = ArithmeticUtils.pow(BigInteger.valueOf(-2L), bits.subtract(BigInteger.ONE));
					final BigInteger upper = ArithmeticUtils.pow(BigInteger.valueOf(2L), bits.subtract(BigInteger.ONE));
					return Integer.Factory.getInstance(lower, upper);
				}
				return Integer.Factory.getInstance(null, null);
			}

			/**
			 * Gets instance of compound {@link SignedByte} type.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 *
			 * @return the newly created compound {@link SignedByte} type
			 */
			public static SignedByte getInstance(final BigInteger bits) {
				return new SignedByteImpl(bits);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof SignedByte);
			}

			@Override
			public java.lang.String toString() {
				return getName();
			}
		}
	}
}

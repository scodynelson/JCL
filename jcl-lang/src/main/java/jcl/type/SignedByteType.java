/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.math.BigInteger;

import jcl.type.typespecifier.AndTypeSpecifier;
import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.util.ArithmeticUtils;

/**
 * The atomic type specifier {@link SignedByteType} denotes the same type as is denoted by the type specifier {@link
 * IntegerType}; however, the list forms of these two type specifiers have different semantics.
 * <p>
 * This denotes the set of {@link IntegerType}s that can be represented in two's-complement form in a byte of s bits.
 * This is equivalent to (integer -2^s-1 2^s-1-1). The type {@link SignedByteType} or the type (signed-byte *) is the
 * same as the type {@link IntegerType}.
 * <p>
 * {@link SignedByteType} -&gt; {@link IntegerType} -&gt; {@link RationalType} -&gt; {@link RealType} -&gt; {@link
 * NumberType} -&gt; {@link TType}
 */
public interface SignedByteType extends IntegerType {

	/**
	 * Singleton instance of the {@link SignedByteType} type.
	 */
	SignedByteType INSTANCE = new Factory.SignedByteTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SignedByteType> {

		/**
		 * Gets instance of compound {@link SignedByteType} type.
		 *
		 * @param bits
		 * 		the bits that comprise the byte value
		 *
		 * @return the newly created compound {@link SignedByteType} type
		 */
		public static SignedByteType getInstance(final BigInteger bits) {
			return SignedByteTypeImpl.getInstance(bits);
		}

		@Override
		public SignedByteType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SignedByteType} type implementation.
		 */
		private static final class SignedByteTypeImpl extends AndTypeSpecifier implements SignedByteType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private SignedByteTypeImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link SignedByteType} type.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 */
			private SignedByteTypeImpl(final BigInteger bits) {
				super("SIGNED-BYTE", getSuper(bits));
			}

			/**
			 * This method gets the {@link IntegerType} supertype for creating a {@link SignedByteType}.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 *
			 * @return the {@link IntegerType} supertype
			 */
			private static IntegerType getSuper(final BigInteger bits) {
				if (bits != null) {
					final BigInteger lower = ArithmeticUtils.pow(BigInteger.valueOf(-2L), bits.subtract(BigInteger.ONE));
					final BigInteger upper = ArithmeticUtils.pow(BigInteger.valueOf(2L), bits.subtract(BigInteger.ONE));
					return IntegerType.Factory.getInstance(lower, upper);
				}
				return IntegerType.Factory.getInstance(null, null);
			}

			/**
			 * Gets instance of compound {@link SignedByteType} type.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 *
			 * @return the newly created compound {@link SignedByteType} type
			 */
			public static SignedByteType getInstance(final BigInteger bits) {
				return new SignedByteTypeImpl(bits);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof SignedByteType);
			}

			@Override
			public String toString() {
				return getName();
			}
		}
	}
}

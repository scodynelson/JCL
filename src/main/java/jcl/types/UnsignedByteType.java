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
 * The atomic type specifier {@link UnsignedByteType} denotes the same type as is denoted by the type specifier
 * (integer 0 *).
 * <p>
 * This denotes the set of non-negative {@link IntegerType}s that can be represented in a byte of size s (bits). This
 * is equivalent to (mod m) for m=2^s, or to (integer 0 n) for n=2^s-1. The type {@link UnsignedByteType} or the type
 * (unsigned-byte *) is the same as the type (integer 0 *), the set of non-negative {@link IntegerType}s.
 * <p>
 * {@link UnsignedByteType} -&gt; {@link SignedByteType} -&gt; {@link IntegerType} -&gt; {@link RationalType} -&gt;
 * {@link RealType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface UnsignedByteType extends SignedByteType {

	/**
	 * Singleton instance of the {@link UnsignedByteType} type.
	 */
	UnsignedByteType INSTANCE = new Factory.UnsignedByteTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<UnsignedByteType> {

		/**
		 * Gets instance of compound {@link UnsignedByteType} type.
		 *
		 * @param bits
		 * 		the bits that comprise the byte value
		 *
		 * @return the newly created compound {@link UnsignedByteType} type
		 */
		public static UnsignedByteType getInstance(final BigInteger bits) {
			return UnsignedByteTypeImpl.getInstance(bits);
		}

		@Override
		public UnsignedByteType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link UnsignedByteType} type implementation.
		 */
		private static final class UnsignedByteTypeImpl extends AndTypeSpecifier implements UnsignedByteType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 7776477100685781384L;

			/**
			 * Private constructor.
			 */
			private UnsignedByteTypeImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@link UnsignedByteType} type.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 */
			private UnsignedByteTypeImpl(final BigInteger bits) {
				super("UNSIGNED-BYTE", getSuper(bits));
			}

			/**
			 * This method gets the {@link IntegerType} supertype for creating an {@link UnsignedByteType}.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 *
			 * @return the {@link IntegerType} supertype
			 */
			private static IntegerType getSuper(final BigInteger bits) {
				if (bits != null) {
					final BigInteger upper = ArithmeticUtils.pow(BigInteger.valueOf(2L), bits.subtract(BigInteger.ONE));
					return IntegerType.Factory.getInstance(BigInteger.ZERO, upper);
				}
				return IntegerType.Factory.getInstance(BigInteger.ZERO, null);
			}

			/**
			 * Gets instance of compound {@link UnsignedByteType} type.
			 *
			 * @param bits
			 * 		the bits that comprise the byte value
			 *
			 * @return the newly created compound {@link UnsignedByteType} type
			 */
			public static UnsignedByteType getInstance(final BigInteger bits) {
				return new UnsignedByteTypeImpl(bits);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof UnsignedByteType);
			}

			@Override
			public String toString() {
				return getName();
			}
		}
	}
}

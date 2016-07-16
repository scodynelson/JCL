/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.math.BigInteger;

import jcl.types.typespecifiers.AndTypeSpecifier;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@link BitType} is equivalent to the type (integer 0 1) and (unsigned-byte 1).
 * <p>
 * {@link BitType} -&gt; {@link UnsignedByteType} -&gt; {@link SignedByteType} -&gt; {@link IntegerType} -&gt; {@link
 * RationalType} -&gt; {@link RealType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface BitType extends UnsignedByteType {

	/**
	 * Singleton instance of the {@link BitType} type.
	 */
	BitType INSTANCE = new Factory.BitTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BitType> {

		@Override
		public BitType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BitType} type implementation.
		 */
		private static final class BitTypeImpl extends AndTypeSpecifier implements BitType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BitTypeImpl() {
				super("BIT", UnsignedByteType.Factory.getInstance(BigInteger.ONE));
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof BitType);
			}

			@Override
			public String toString() {
				return getName();
			}
		}
	}
}

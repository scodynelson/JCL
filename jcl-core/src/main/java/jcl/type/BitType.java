/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.math.BigInteger;

import jcl.type.typespecifier.AndTypeSpecifier;
import jcl.type.typespecifier.AtomicTypeSpecifier;

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
			public boolean typeEquals(final Object obj) {
				// TODO: Hack for FixnumType until type system gets reworked
				return (this == obj) || (obj instanceof BitType) || (obj instanceof FixnumType);
			}

			@Override
			public String toString() {
				return getName();
			}
		}
	}
}

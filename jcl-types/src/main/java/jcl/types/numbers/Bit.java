package jcl.types.numbers;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.compound.AndTypeSpecifier;

import java.math.BigInteger;

/**
 * The type {@code Bit} is equivalent to the type (integer 0 1) and (unsigned-byte 1).
 */
public interface Bit extends UnsignedByte, SignedByte, Integer, Rational, Real, Number, T {

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
		 * Inner {@code Bit} type implementation.
		 */
		private static class BitImpl extends AndTypeSpecifier implements Bit, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BitImpl() {
				super(UnsignedByte.Factory.getInstance(BigInteger.ONE));
			}

			@Override
			public boolean equals(final Object obj) {
				return super.equals(obj) || (obj == INSTANCE);
			}
		}
	}
}

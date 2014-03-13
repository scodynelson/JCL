package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.compound.AndTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigInteger;

/**
 * The type {@code Bit} is equivalent to the type (integer 0 1) and (unsigned-byte 1).
 * <p/>
 * {@code Bit} -> {@code UnsignedByte} -> {@code SignedByte} -> {@code Integer} -> {@code Rational} -> {@code Real} -> {@code Number} -> {@code T}
 */
public interface Bit extends UnsignedByte {

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

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

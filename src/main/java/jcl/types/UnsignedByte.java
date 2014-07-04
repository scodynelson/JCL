package jcl.types;

import jcl.typespecifiers.AndTypeSpecifier;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.util.ArithmeticUtils;

import java.lang.String;
import java.math.BigInteger;

/**
 * The atomic type specifier {@code UnsignedByte} denotes the same type as is denoted by the type specifier (integer 0 *).
 * <p>
 * This denotes the set of non-negative {@code Integer}s that can be represented in a byte of size s (bits). This is
 * equivalent to (mod m) for m=2^s, or to (integer 0 n) for n=2^s-1. The type {@code UnsignedByte} or the type
 * (unsigned-byte *) is the same as the type (integer 0 *), the set of non-negative {@code Integer}s.
 * <p>
 * {@code UnsignedByte} -> {@code SignedByte} -> {@code Integer} -> {@code Rational} -> {@code Real} -> {@code Number} -> {@code T}
 */
public interface UnsignedByte extends SignedByte {

	UnsignedByte INSTANCE = new Factory.UnsignedByteImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<UnsignedByte> {

		@Override
		public UnsignedByte getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code UnsignedByte} type.
		 *
		 * @param bits the bits that comprise the byte value
		 * @return the newly created compound {@code UnsignedByte} type
		 */
		public static UnsignedByte getInstance(final BigInteger bits) {
			return UnsignedByteImpl.getInstance(bits);
		}

		/**
		 * Inner {@code UnsignedByte} type implementation.
		 */
		private static class UnsignedByteImpl extends AndTypeSpecifier implements UnsignedByte, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private UnsignedByteImpl() {
				this(null);
			}

			/**
			 * Private constructor for compound {@code UnsignedByte} type.
			 *
			 * @param bits the bits that comprise the byte value
			 */
			private UnsignedByteImpl(final BigInteger bits) {
				super(getSuper(bits));
			}

			/**
			 * This method gets the {@code Integer} supertype for creating an {@code UnsignedByte}.
			 *
			 * @param bits the bits that comprise the byte value
			 * @return the {@code Integer} supertype
			 */
			private static Integer getSuper(final BigInteger bits) {
				if (bits != null) {
					final BigInteger upper = ArithmeticUtils.pow(BigInteger.valueOf(2L), bits.subtract(BigInteger.ONE));
					return Integer.Factory.getInstance(BigInteger.ZERO, upper);
				}
				return Integer.Factory.getInstance(BigInteger.ZERO, null);
			}

			/**
			 * Gets instance of compound {@code UnsignedByte} type.
			 *
			 * @param bits the bits that comprise the byte value
			 * @return the newly created compound {@code UnsignedByte} type
			 */
			public static UnsignedByte getInstance(final BigInteger bits) {
				return new UnsignedByteImpl(bits);
			}

			@Override
			public boolean equals(final Object obj) {
				return super.equals(obj) || (obj == INSTANCE);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "UnsignedByteImpl{}";
			}
		}
	}
}
package jcl.types;

import jcl.typespecifiers.AndTypeSpecifier;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.util.ArithmeticUtils;

import java.lang.String;
import java.math.BigInteger;

/**
 * The atomic type specifier {@link SignedByte} denotes the same type as is denoted by the type specifier {@link
 * Integer}; however, the list forms of these two type specifiers have different semantics.
 * <p>
 * This denotes the set of {@link Integer}s that can be represented in two's-complement form in a byte of s bits. This
 * is equivalent to (integer -2^s-1 2^s-1-1). The type {@link SignedByte} or the type (signed-byte *) is the same as
 * the type {@link Integer}.
 * <p>
 * {@link SignedByte} -> {@link Integer} -> {@link Rational} -> {@link Real} -> {@link Number} -> {@link T}
 */
public interface SignedByte extends Integer {

	SignedByte INSTANCE = new Factory.SignedByteImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SignedByte> {

		@Override
		public SignedByte getInstance() {
			return INSTANCE;
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
			return SignedByteImpl.getInstance(bits);
		}

		/**
		 * Inner {@link SignedByte} type implementation.
		 */
		private static class SignedByteImpl extends AndTypeSpecifier implements SignedByte, AtomicTypeSpecifier {

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
			public boolean equals(final Object obj) {
				return super.equals(obj) || (obj == INSTANCE);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

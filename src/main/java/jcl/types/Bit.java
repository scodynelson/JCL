package jcl.types;

import jcl.typespecifiers.AndTypeSpecifier;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;
import java.math.BigInteger;

/**
 * The type {@link Bit} is equivalent to the type (integer 0 1) and (unsigned-byte 1).
 * <p>
 * {@link Bit} -> {@link UnsignedByte} -> {@link SignedByte} -> {@link Integer} -> {@link Rational} -> {@link Real} ->
 * {@link Number} -> {@link T}
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
		 * Inner {@link Bit} type implementation.
		 */
		private static final class BitImpl extends AndTypeSpecifier implements Bit, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BitImpl() {
				super("BIT", UnsignedByte.Factory.getInstance(BigInteger.ONE));
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

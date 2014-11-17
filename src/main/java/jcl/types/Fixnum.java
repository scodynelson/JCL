package jcl.types;

import jcl.typespecifiers.AndTypeSpecifier;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;
import java.math.BigInteger;

/**
 * A {@link Fixnum} is an {@link Integer} whose value is between most-negative-fixnum and most-positive-fixnum
 * inclusive. The type {@link Fixnum} is a supertype of (signed-byte 16).
 * <p>
 * {@link Fixnum} -> {@link Integer} -> {@link Rational} -> {@link Real} -> {@link Number} -> {@link T}
 */
public interface Fixnum extends Integer {

	Fixnum INSTANCE = new Factory.FixnumImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Fixnum> {

		@Override
		public Fixnum getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Fixnum} type implementation.
		 */
		private static final class FixnumImpl extends AndTypeSpecifier implements Fixnum, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private FixnumImpl() {
				super("FIXNUM", getSuper());
			}

			/**
			 * This method gets the {@link Integer} supertype for creating a {@link Fixnum}.
			 *
			 * @return the {@link Integer} supertype
			 */
			private static Integer getSuper() {
				return Integer.Factory.getInstance(BigInteger.valueOf(java.lang.Integer.MIN_VALUE), BigInteger.valueOf(java.lang.Integer.MAX_VALUE));
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

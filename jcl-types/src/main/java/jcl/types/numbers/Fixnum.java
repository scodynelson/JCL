package jcl.types.numbers;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.compound.AndTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigInteger;

/**
 * A {@code Fixnum} is an {@code Integer} whose value is between most-negative-fixnum and most-positive-fixnum inclusive.
 * The type {@code Fixnum} is a supertype of (signed-byte 16).
 * <p/>
 * {@code Fixnum} -> {@code Integer} -> {@code Rational} -> {@code Real} -> {@code Number} -> {@code T}
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
		 * Inner {@code Fixnum} type implementation.
		 */
		private static class FixnumImpl extends AndTypeSpecifier implements Fixnum, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private FixnumImpl() {
				super(getSuper());
			}

			/**
			 * This method gets the {@code Integer} supertype for creating a {@code Fixnum}.
			 *
			 * @return the {@code Integer} supertype
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
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

package jcl.types;

import jcl.types.typespecifiers.AndTypeSpecifier;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.NotTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * The type {@link Bignum} is defined to be exactly (and integer (not fixnum)).
 * <p>
 * {@link Bignum} -> {@link Integer} -> {@link Rational} -> {@link Real} -> {@link Number} -> {@link T}
 */
public interface Bignum extends Integer {

	Bignum INSTANCE = new Factory.BignumImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Bignum> {

		@Override
		public Bignum getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Bignum} type implementation.
		 */
		private static final class BignumImpl extends AndTypeSpecifier implements Bignum, AtomicTypeSpecifier {

			private static final long serialVersionUID = -2641268480714921916L;

			/**
			 * Private constructor.
			 */
			private BignumImpl() {
				super("BIGNUM", Integer.INSTANCE, new NotTypeSpecifier(Fixnum.INSTANCE));
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

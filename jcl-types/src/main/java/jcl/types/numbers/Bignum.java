package jcl.types.numbers;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.compound.AndTypeSpecifier;
import jcl.types.typespecifiers.compound.NotTypeSpecifier;

/**
 * The type {@code Bignum} is defined to be exactly (and integer (not fixnum)).
 */
public interface Bignum extends Integer, Rational, Real, Number, T {

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
		 * Inner {@code Bignum} type implementation.
		 */
		private static class BignumImpl extends AndTypeSpecifier implements Bignum, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BignumImpl() {
				super(Integer.INSTANCE, new NotTypeSpecifier(Fixnum.INSTANCE));
			}

			@Override
			public boolean equals(final Object obj) {
				return super.equals(obj) || (obj == INSTANCE);
			}
		}
	}
}

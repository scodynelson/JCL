/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AndTypeSpecifier;
import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.NotTypeSpecifier;

/**
 * The type {@link BignumType} is defined to be exactly (and integer (not fixnum)).
 * <p>
 * {@link BignumType} -&gt; {@link IntegerType} -&gt; {@link RationalType} -&gt; {@link RealType} -&gt; {@link
 * NumberType} -&gt; {@link TType}
 */
public interface BignumType extends IntegerType {

	/**
	 * Singleton instance of the {@link BignumType} type.
	 */
	BignumType INSTANCE = new Factory.BignumTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BignumType> {

		@Override
		public BignumType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BignumType} type implementation.
		 */
		private static final class BignumTypeImpl extends AndTypeSpecifier implements BignumType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BignumTypeImpl() {
				super("BIGNUM", IntegerType.INSTANCE, new NotTypeSpecifier(FixnumType.INSTANCE));
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof BignumType);
			}

			@Override
			public String toString() {
				return getName();
			}
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.math.BigInteger;

import jcl.types.typespecifiers.AndTypeSpecifier;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link FixnumType} is an {@link IntegerType} whose value is between most-negative-fixnum and most-positive-fixnum
 * inclusive. The type {@link FixnumType} is a supertype of (signed-byte 16).
 * <p>
 * {@link FixnumType} -&gt; {@link IntegerType} -&gt; {@link RationalType} -&gt; {@link RealType} -&gt; {@link
 * NumberType} -&gt; {@link TType}
 */
public interface FixnumType extends IntegerType {

	/**
	 * Singleton instance of the {@link FixnumType} type.
	 */
	FixnumType INSTANCE = new Factory.FixnumTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<FixnumType> {

		@Override
		public FixnumType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link FixnumType} type implementation.
		 */
		private static final class FixnumTypeImpl extends AndTypeSpecifier implements FixnumType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 5001042603403390586L;

			/**
			 * Private constructor.
			 */
			private FixnumTypeImpl() {
				super("FIXNUM", getSuper());
			}

			/**
			 * This method gets the {@link IntegerType} supertype for creating a {@link FixnumType}.
			 *
			 * @return the {@link IntegerType} supertype
			 */
			private static IntegerType getSuper() {
				return IntegerType.Factory.getInstance(BigInteger.valueOf(Integer.MIN_VALUE), BigInteger.valueOf(Integer.MAX_VALUE));
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof FixnumType);
			}

			@Override
			public String toString() {
				return getName();
			}
		}
	}
}

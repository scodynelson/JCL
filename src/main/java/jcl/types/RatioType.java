/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link RatioType} is a {@link NumberType} representing the mathematical ratio of two non-zero integers, the
 * numerator and denominator, whose greatest common divisor is one, and of which the denominator is positive and
 * greater than one.
 * <p>
 * {@link RatioType} -&gt; {@link RationalType} -&gt; {@link RealType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface RatioType extends RationalType {

	/**
	 * Singleton instance of the {@link RatioType} type.
	 */
	RatioType INSTANCE = new Factory.RatioTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<RatioType> {

		@Override
		public RatioType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link RatioType} type implementation.
		 */
		private static final class RatioTypeImpl extends TypeBaseClass implements RatioType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private RatioTypeImpl() {
				super("RATIO");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof RatioType);
			}
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link RandomStateType} object contains state information used by the pseudo-random number generator.
 * <p>
 * {@link RandomStateType} -&gt; {@link TType}
 */
public interface RandomStateType extends TType {

	/**
	 * Singleton instance of the {@link RandomStateType} type.
	 */
	RandomStateType INSTANCE = new Factory.RandomStateTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<RandomStateType> {

		@Override
		public RandomStateType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link RandomStateType} type implementation.
		 */
		private static final class RandomStateTypeImpl extends TypeBaseClass implements RandomStateType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private RandomStateTypeImpl() {
				super("RANDOM-STATE");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof RandomStateType);
			}
		}
	}
}

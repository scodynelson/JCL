/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link RandomState} object contains state information used by the pseudo-random number generator.
 * <p>
 * {@link RandomState} -&gt; {@link T}
 */
public interface RandomState extends T {

	/**
	 * Singleton instance of the {@link RandomState} type.
	 */
	RandomState INSTANCE = new Factory.RandomStateImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<RandomState> {

		@Override
		public RandomState getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link RandomState} type implementation.
		 */
		private static final class RandomStateImpl extends TypeBaseClass implements RandomState, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -8325573429698690966L;

			/**
			 * Private constructor.
			 */
			private RandomStateImpl() {
				super("RANDOM-STATE");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof RandomState);
			}
		}
	}
}

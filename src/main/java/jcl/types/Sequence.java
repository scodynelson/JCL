/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link Sequence} is an ordered collection of objects, called the elements of the sequence. The type {@link Vector}
 * and the type {@link List} are disjoint subtypes of type {@link Sequence}, but are not necessarily an exhaustive
 * partition of the {@link Sequence}. When viewing a {@link Vector} as a {@link Sequence}, only the active elements of
 * that {@link Vector} are considered elements of the {@link Sequence}; that is, {@link Sequence} operations respect
 * the fill pointer when the given {@link Sequence} represents a {@link Vector}.
 * <p>
 * {@link Sequence} -&gt; {@link T}
 */
public interface Sequence extends T {

	/**
	 * Singleton instance of the {@link Sequence} type.
	 */
	Sequence INSTANCE = new Factory.SequenceImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Sequence> {

		@Override
		public Sequence getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Sequence} type implementation.
		 */
		private static final class SequenceImpl extends TypeBaseClass implements Sequence, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -4924814075712876096L;

			/**
			 * Private constructor.
			 */
			private SequenceImpl() {
				super("SEQUENCE");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Sequence);
			}
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link SequenceType} is an ordered collection of objects, called the elements of the sequence. The type {@link
 * VectorType} and the type {@link ListType} are disjoint subtypes of type {@link SequenceType}, but are not
 * necessarily an exhaustive partition of the {@link SequenceType}. When viewing a {@link VectorType} as a {@link
 * SequenceType}, only the active elements of that {@link VectorType} are considered elements of the {@link
 * SequenceType}; that is, {@link SequenceType} operations respect the fill pointer when the given {@link SequenceType}
 * represents a {@link VectorType}.
 * <p>
 * {@link SequenceType} -&gt; {@link TType}
 */
public interface SequenceType extends TType {

	/**
	 * Singleton instance of the {@link SequenceType} type.
	 */
	SequenceType INSTANCE = new Factory.SequenceTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SequenceType> {

		@Override
		public SequenceType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SequenceType} type implementation.
		 */
		private static final class SequenceTypeImpl extends TypeBaseClass implements SequenceType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private SequenceTypeImpl() {
				super("SEQUENCE");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof SequenceType);
			}
		}
	}
}

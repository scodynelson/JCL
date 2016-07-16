/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.NotTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * An {@link AtomType} is a type equivalent to (not cons).
 * <p>
 * {@link AtomType} -&gt; {@link TType}
 */
public interface AtomType extends TType {

	/**
	 * Singleton instance of the {@link AtomType} type.
	 */
	AtomType INSTANCE = new Factory.AtomTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<AtomType> {

		@Override
		public AtomType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link AtomType} type implementation.
		 */
		private static final class AtomTypeImpl extends NotTypeSpecifier implements AtomType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private AtomTypeImpl() {
				super("ATOM", ConsType.INSTANCE);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof AtomType);
			}

			@Override
			public String toString() {
				return getName();
			}
		}
	}
}

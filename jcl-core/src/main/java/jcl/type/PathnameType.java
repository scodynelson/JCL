/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link PathnameType} is a structured object which represents a filename.
 * <p>
 * There are two kinds of {@link PathnameType}s---physical pathnames and {@link LogicalPathnameType}s.
 * <p>
 * {@link PathnameType} -&gt; {@link TType}
 */
public interface PathnameType extends TType {

	/**
	 * Singleton instance of the {@link PathnameType} type.
	 */
	PathnameType INSTANCE = new Factory.PathnameTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<PathnameType> {

		@Override
		public PathnameType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link PathnameType} type implementation.
		 */
		private static final class PathnameTypeImpl extends TypeBaseClass implements PathnameType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private PathnameTypeImpl() {
				super("PATHNAME");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof PathnameType);
			}
		}
	}
}

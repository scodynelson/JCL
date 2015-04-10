/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.LispType;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@link TType} is the set of all objects. It is a supertype of every type, including itself. Every object is
 * of type {@link TType}.
 * <p>
 * {@link TType}
 */
public interface TType extends LispType {

	/**
	 * Singleton instance of the {@link TType} type.
	 */
	TType INSTANCE = new Factory.TTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<TType> {

		@Override
		public TType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link TType} type implementation.
		 */
		private static final class TTypeImpl extends TypeBaseClass implements TType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -5539110546772298423L;

			/**
			 * Private constructor.
			 */
			private TTypeImpl() {
				super("T");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof TType);
			}
		}
	}
}

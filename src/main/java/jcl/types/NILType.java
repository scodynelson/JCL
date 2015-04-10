/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@link NILType} contains no objects and so is also called the empty type. The type {@link NILType} is a
 * subtype of every type. No object is of type {@link NILType}.
 * <p>
 * The type containing the object {@link NILType} is the type {@link NullType}, not the type {@link NILType}.
 * <p>
 * {@link NILType}
 */
public interface NILType extends TType { // TODO: this needs to extend ALL types...

	/**
	 * Singleton instance of the {@link NILType} type.
	 */
	NILType INSTANCE = new Factory.NILTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<NILType> {

		@Override
		public NILType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link NILType} type implementation.
		 */
		private static final class NILTypeImpl extends TypeBaseClass implements NILType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 7666156319177181978L;

			/**
			 * Private constructor.
			 */
			private NILTypeImpl() {
				super("NIL");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof NILType);
			}
		}
	}
}

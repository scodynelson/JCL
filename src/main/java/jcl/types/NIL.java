/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@link NIL} contains no objects and so is also called the empty type. The type {@link NIL} is a subtype of
 * every type. No object is of type {@link NIL}.
 * <p>
 * The type containing the object {@link NIL} is the type {@link Null}, not the type {@link NIL}.
 * <p>
 * {@link NIL}
 */
public interface NIL extends T { // TODO: this needs to extend ALL types...

	/**
	 * Singleton instance of the {@link NIL} type.
	 */
	NIL INSTANCE = new Factory.NILImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<NIL> {

		@Override
		public NIL getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link NIL} type implementation.
		 */
		private static final class NILImpl extends TypeBaseClass implements NIL, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 7666156319177181978L;

			/**
			 * Private constructor.
			 */
			private NILImpl() {
				super("NIL");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof NIL);
			}
		}
	}
}

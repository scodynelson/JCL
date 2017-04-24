/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * The type {@link NILType} contains no objects and so is also called the empty type. The type {@link NILType} is a
 * subtype of every type. No object is of type {@link NILType}.
 * <p>
 * The type containing the object {@link NILType} is the type {@link NullType}, not the type {@link NILType}.
 * <p>
 * {@link NILType}
 */
public interface NILType extends TType, ListType { // TODO: this needs to extend ALL types...

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
			 * Private constructor.
			 */
			private NILTypeImpl() {
				super("NIL");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof NILType);
			}
		}
	}
}

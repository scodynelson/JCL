/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * The {@link StandardObjectType} is an instance of {@link StandardClassType} and is a superclass of every {@link
 * ClassType} that is an instance of {@link StandardClassType} except itself.
 * <p>
 * {@link StandardObjectType} -&gt; {@link TType}
 */
public interface StandardObjectType extends TType {

	/**
	 * Singleton instance of the {@link StandardObjectType} type.
	 */
	StandardObjectType INSTANCE = new Factory.StandardObjectTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardObjectType> {

		@Override
		public StandardObjectType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardObjectType} type implementation.
		 */
		private static final class StandardObjectTypeImpl extends TypeBaseClass implements StandardObjectType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StandardObjectTypeImpl() {
				super("STANDARD-OBJECT");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof StandardObjectType);
			}
		}
	}
}

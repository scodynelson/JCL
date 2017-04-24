/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * A {@link StandardMethodType} is the default method type {@link ClassType}.
 * <p>
 * {@link StandardMethodType} -&gt; {@link MethodType} -&gt; {@link StandardObjectType} -&gt; {@link TType}
 */
public interface StandardMethodType extends MethodType, StandardObjectType {

	/**
	 * Singleton instance of the {@link StandardMethodType} type.
	 */
	StandardMethodType INSTANCE = new Factory.StandardMethodTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardMethodType> {

		@Override
		public StandardMethodType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardMethodType} type implementation.
		 */
		private static final class StandardMethodTypeImpl extends TypeBaseClass implements StandardMethodType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StandardMethodTypeImpl() {
				super("STANDARD-METHOD");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof StandardMethodType);
			}
		}
	}
}

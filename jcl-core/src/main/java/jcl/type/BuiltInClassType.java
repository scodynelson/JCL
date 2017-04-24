/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * A {@link BuiltInClassType} is a {@link ClassType} whose instances have restricted capabilities or special
 * representations. {@link BuiltInClassType}es can be used as parameter specializers in {@link MethodType}s.
 * <p>
 * {@link BuiltInClassType} -&gt; {@link ClassType} -&gt; {@link StandardObjectType} -&gt; {@link TType}
 */
public interface BuiltInClassType extends ClassType {

	/**
	 * Singleton instance of the {@link BuiltInClassType} type.
	 */
	BuiltInClassType INSTANCE = new Factory.BuiltInClassTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BuiltInClassType> {

		@Override
		public BuiltInClassType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BuiltInClassType} type implementation.
		 */
		private static final class BuiltInClassTypeImpl extends TypeBaseClass implements BuiltInClassType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BuiltInClassTypeImpl() {
				super("BUILT-IN-CLASS");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof BuiltInClassType);
			}
		}
	}
}

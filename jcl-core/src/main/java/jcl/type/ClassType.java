/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * The type {@link ClassType} represents objects that determine the structure and behavior of their instances.
 * Associated with an object of type {@link ClassType} is information describing its place in the directed acyclic
 * graph of classes, its slots, and its options.
 * <p>
 * {@link ClassType} -&gt; {@link StandardObjectType} -&gt; {@link TType}
 */
public interface ClassType extends StandardObjectType {

	/**
	 * Singleton instance of the {@link ClassType} type.
	 */
	ClassType INSTANCE = new Factory.ClassTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ClassType> {

		@Override
		public ClassType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ClassType} type implementation.
		 */
		private static final class ClassTypeImpl extends TypeBaseClass implements ClassType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private ClassTypeImpl() {
				super("CLASS");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof ClassType);
			}
		}
	}
}

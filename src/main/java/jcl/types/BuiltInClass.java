/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link BuiltInClass} is a {@link Class} whose instances have restricted capabilities or special representations.
 * {@link BuiltInClass}es can be used as parameter specializers in {@link Method}s.
 * <p>
 * {@link BuiltInClass} -&gt; {@link Class} -&gt; {@link StandardObject} -&gt; {@link T}
 */
public interface BuiltInClass extends Class {

	/**
	 * Singleton instance of the {@link BuiltInClass} type.
	 */
	BuiltInClass INSTANCE = new Factory.BuiltInClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BuiltInClass> {

		@Override
		public BuiltInClass getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BuiltInClass} type implementation.
		 */
		private static final class BuiltInClassImpl extends TypeBaseClass implements BuiltInClass, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -3777422495647309699L;

			/**
			 * Private constructor.
			 */
			private BuiltInClassImpl() {
				super("BUILT-IN-CLASS");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof BuiltInClass);
			}
		}
	}
}

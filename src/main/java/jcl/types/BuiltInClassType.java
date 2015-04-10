/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

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
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -3777422495647309699L;

			/**
			 * Private constructor.
			 */
			private BuiltInClassTypeImpl() {
				super("BUILT-IN-CLASS");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof BuiltInClassType);
			}
		}
	}
}

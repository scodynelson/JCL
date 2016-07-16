/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link GenericFunctionType} is a {@link FunctionType} whose behavior depends on the {@link ClassType}es or
 * identities of the arguments supplied to it. A {@link GenericFunctionType} object contains a set of {@link
 * MethodType}s, a lambda list, a {@link MethodCombinationType} type, and other information. The {@link MethodType}s
 * define the class-specific behavior and operations of the {@link GenericFunctionType}; a {@link MethodType} is said
 * to specialize a {@link GenericFunctionType}. When invoked, a {@link GenericFunctionType} executes a subset of its
 * {@link MethodType}s based on the {@link ClassType}es or identities of its arguments.
 * <p>
 * A {@link GenericFunctionType} can be used in the same ways that an ordinary {@link FunctionType} can be used.
 * <p>
 * {@link GenericFunctionType} -&gt; {@link FunctionType} -&gt; {@link TType}
 */
public interface GenericFunctionType extends FunctionType {

	/**
	 * Singleton instance of the {@link GenericFunctionType} type.
	 */
	GenericFunctionType INSTANCE = new Factory.GenericFunctionTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<GenericFunctionType> {

		@Override
		public GenericFunctionType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link GenericFunctionType} type implementation.
		 */
		private static final class GenericFunctionTypeImpl extends TypeBaseClass implements GenericFunctionType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private GenericFunctionTypeImpl() {
				super("GENERIC-FUNCTION");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof GenericFunctionType);
			}
		}
	}
}

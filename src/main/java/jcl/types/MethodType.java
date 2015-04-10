/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link MethodType} is an object that represents a modular part of the behavior of a {@link GenericFunctionType}.
 * <p>
 * A {@link MethodType} contains code to implement the {@link MethodType}'s behavior, a sequence of parameter
 * specializers that specify when the given {@link MethodType} is applicable, and a sequence of qualifiers that is used
 * by the {@link MethodCombinationType} facility to distinguish among {@link MethodType}s. Each required parameter of
 * each {@link MethodType} has an associated parameter specializer, and the {@link MethodType} will be invoked only on
 * arguments that satisfy its parameter specializers.
 * <p>
 * The {@link MethodCombinationType} facility controls the selection of {@link MethodType}s, the order in which they
 * are run, and the values that are returned by the {@link GenericFunctionType}. The object system offers a default
 * {@link MethodCombinationType} type and provides a facility for declaring new types of {@link MethodCombinationType}.
 * <p>
 * {@link MethodType} -&gt; {@link TType}
 */
public interface MethodType extends TType {

	/**
	 * Singleton instance of the {@link MethodType} type.
	 */
	MethodType INSTANCE = new Factory.MethodTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<MethodType> {

		@Override
		public MethodType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link MethodType} type implementation.
		 */
		private static final class MethodTypeImpl extends TypeBaseClass implements MethodType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 3888222951043301596L;

			/**
			 * Private constructor.
			 */
			private MethodTypeImpl() {
				super("METHOD");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof MethodType);
			}
		}
	}
}

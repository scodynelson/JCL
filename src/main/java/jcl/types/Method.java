/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link Method} is an object that represents a modular part of the behavior of a {@link GenericFunction}.
 * <p>
 * A {@link Method} contains code to implement the {@link Method}'s behavior, a sequence of parameter specializers that
 * specify when the given {@link Method} is applicable, and a sequence of qualifiers that is used by the {@link
 * MethodCombination} facility to distinguish among {@link Method}s. Each required parameter of each {@link Method} has
 * an associated parameter specializer, and the {@link Method} will be invoked only on arguments that satisfy its
 * parameter specializers.
 * <p>
 * The {@link MethodCombination} facility controls the selection of {@link Method}s, the order in which they are run,
 * and the values that are returned by the {@link GenericFunction}. The object system offers a default {@link
 * MethodCombination} type and provides a facility for declaring new types of {@link MethodCombination}.
 * <p>
 * {@link Method} -&gt; {@link T}
 */
public interface Method extends T {

	/**
	 * Singleton instance of the {@link Method} type.
	 */
	Method INSTANCE = new Factory.MethodImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Method> {

		@Override
		public Method getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Method} type implementation.
		 */
		private static final class MethodImpl extends TypeBaseClass implements Method, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 3888222951043301596L;

			/**
			 * Private constructor.
			 */
			private MethodImpl() {
				super("METHOD");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Method);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}

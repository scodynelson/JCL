/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link MethodCombinationType} object represents the information about the {@link MethodCombinationType} being used
 * by a {@link GenericFunctionType}. A {@link MethodCombinationType} object contains information about both the type of
 * {@link MethodCombinationType} and the arguments being used with that type.
 * <p>
 * {@link MethodCombinationType} -&gt; {@link TType}
 */
public interface MethodCombinationType extends TType {

	/**
	 * Singleton instance of the {@link MethodCombinationType} type.
	 */
	MethodCombinationType INSTANCE = new Factory.MethodCombinationTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<MethodCombinationType> {

		@Override
		public MethodCombinationType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link MethodCombinationType} type implementation.
		 */
		private static final class MethodCombinationTypeImpl extends TypeBaseClass implements MethodCombinationType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 3032946625869229276L;

			/**
			 * Private constructor.
			 */
			private MethodCombinationTypeImpl() {
				super("METHOD-COMBINATION");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof MethodCombinationType);
			}
		}
	}
}

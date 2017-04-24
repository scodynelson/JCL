/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

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
			 * Private constructor.
			 */
			private MethodCombinationTypeImpl() {
				super("METHOD-COMBINATION");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof MethodCombinationType);
			}
		}
	}
}

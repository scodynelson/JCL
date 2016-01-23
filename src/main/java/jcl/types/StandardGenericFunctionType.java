/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The class {@link StandardGenericFunctionType} is the default {@link ClassType} of {@link GenericFunctionType}s.
 * <p>
 * {@link StandardGenericFunctionType} -&gt; {@link GenericFunctionType} -&gt; {@link FunctionType} -&gt; {@link TType}
 */
public interface StandardGenericFunctionType extends GenericFunctionType {

	/**
	 * Singleton instance of the {@link StandardGenericFunctionType} type.
	 */
	StandardGenericFunctionType INSTANCE = new Factory.StandardGenericFunctionTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardGenericFunctionType> {

		@Override
		public StandardGenericFunctionType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardGenericFunctionType} type implementation.
		 */
		private static final class StandardGenericFunctionTypeImpl extends TypeBaseClass implements StandardGenericFunctionType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StandardGenericFunctionTypeImpl() {
				super("STANDARD-GENERIC-FUNCTION");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardGenericFunctionType);
			}
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link StandardMethodType} is the default method type {@link ClassType}.
 * <p>
 * {@link StandardMethodType} -&gt; {@link MethodType} -&gt; {@link StandardObjectType} -&gt; {@link TType}
 */
public interface StandardMethodType extends MethodType, StandardObjectType {

	/**
	 * Singleton instance of the {@link StandardMethodType} type.
	 */
	StandardMethodType INSTANCE = new Factory.StandardMethodTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardMethodType> {

		@Override
		public StandardMethodType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardMethodType} type implementation.
		 */
		private static final class StandardMethodTypeImpl extends TypeBaseClass implements StandardMethodType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StandardMethodTypeImpl() {
				super("STANDARD-METHOD");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardMethodType);
			}
		}
	}
}

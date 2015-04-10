/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link StandardClassType} is the default class type {@link ClassType}.
 * <p>
 * {@link StandardClassType} -&gt; {@link ClassType} -&gt; {@link StandardObjectType} -&gt; {@link TType}
 */
public interface StandardClassType extends ClassType {

	/**
	 * Singleton instance of the {@link StandardClassType} type.
	 */
	StandardClassType INSTANCE = new Factory.StandardClassTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardClassType> {

		@Override
		public StandardClassType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardClassType} type implementation.
		 */
		private static final class StandardClassTypeImpl extends TypeBaseClass implements StandardClassType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 8869486055336833896L;

			/**
			 * Private constructor.
			 */
			private StandardClassTypeImpl() {
				super("STANDARD-CLASS");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardClassType);
			}
		}
	}
}

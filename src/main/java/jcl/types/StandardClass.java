/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link StandardClass} is the default class type {@link Class}.
 * <p>
 * {@link StandardClass} -> {@link Class} -> {@link StandardObject} -> {@link T}
 */
public interface StandardClass extends Class {

	/**
	 * Singleton instance of the {@link StandardClass} type.
	 */
	StandardClass INSTANCE = new Factory.StandardClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardClass> {

		@Override
		public StandardClass getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardClass} type implementation.
		 */
		private static final class StandardClassImpl extends TypeBaseClass implements StandardClass, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 8869486055336833896L;

			/**
			 * Private constructor.
			 */
			private StandardClassImpl() {
				super("STANDARD-CLASS");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardClass);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}

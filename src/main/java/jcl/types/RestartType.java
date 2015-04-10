/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link RestartType} represents a {@link FunctionType} that can be called to perform some form of recovery action,
 * usually a transfer of control to an outer point in the running program.
 * <p>
 * A {@link RestartType} has only dynamic extent relative to the scope of the binding form which establishes it.
 * <p>
 * {@link RestartType} -&gt; {@link TType}
 */
public interface RestartType extends TType {

	/**
	 * Singleton instance of the {@link RestartType} type.
	 */
	RestartType INSTANCE = new Factory.RestartTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<RestartType> {

		@Override
		public RestartType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link RestartType} type implementation.
		 */
		private static final class RestartTypeImpl extends TypeBaseClass implements RestartType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -7905323000782513518L;

			/**
			 * Private constructor.
			 */
			private RestartTypeImpl() {
				super("RESTART");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof RestartType);
			}
		}
	}
}

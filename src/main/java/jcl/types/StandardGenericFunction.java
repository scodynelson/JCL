/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * The class {@link StandardGenericFunction} is the default {@link Class} of {@link GenericFunction}s.
 * <p>
 * {@link StandardGenericFunction} -&gt; {@link GenericFunction} -&gt; {@link Function} -&gt; {@link T}
 */
public interface StandardGenericFunction extends GenericFunction {

	/**
	 * Singleton instance of the {@link StandardGenericFunction} type.
	 */
	StandardGenericFunction INSTANCE = new Factory.StandardGenericFunctionImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardGenericFunction> {

		@Override
		public StandardGenericFunction getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardGenericFunction} type implementation.
		 */
		private static final class StandardGenericFunctionImpl extends TypeBaseClass implements StandardGenericFunction, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -4167796389671311751L;

			/**
			 * Private constructor.
			 */
			private StandardGenericFunctionImpl() {
				super("STANDARD-GENERIC-FUNCTION");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardGenericFunction);
			}

			@Override
			public String toString() {
//				return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
				return getName();
			}
		}
	}
}

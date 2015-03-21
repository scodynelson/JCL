/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link CompiledFunction} is any function that contains no references to macros that must be expanded at run time,
 * and contains no unresolved references to load time values.
 * <p>
 * {@link Function}s whose definitions appear lexically within a file that has been compiled and then loaded are of
 * type {@link CompiledFunction}.
 * <p>
 * {@link CompiledFunction} -&gt; {@link Function} -&gt; {@link T}
 */
public interface CompiledFunction extends Function {

	/**
	 * Singleton instance of the {@link CompiledFunction} type.
	 */
	CompiledFunction INSTANCE = new Factory.CompiledFunctionImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<CompiledFunction> {

		@Override
		public CompiledFunction getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link CompiledFunction} type implementation.
		 */
		private static final class CompiledFunctionImpl extends TypeBaseClass implements CompiledFunction, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 7064731489248744673L;

			/**
			 * Private constructor.
			 */
			private CompiledFunctionImpl() {
				super("COMPILED-FUNCTION");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof CompiledFunction);
			}

			@Override
			public String toString() {
//				return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
				return getName();
			}
		}
	}
}

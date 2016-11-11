/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link CompiledFunctionType} is any function that contains no references to macros that must be expanded at run
 * time, and contains no unresolved references to load time values.
 * <p>
 * {@link FunctionType}s whose definitions appear lexically within a file that has been compiled and then loaded are of
 * type {@link CompiledFunctionType}.
 * <p>
 * {@link CompiledFunctionType} -&gt; {@link FunctionType} -&gt; {@link TType}
 */
public interface CompiledFunctionType extends FunctionType {

	/**
	 * Singleton instance of the {@link CompiledFunctionType} type.
	 */
	CompiledFunctionType INSTANCE = new Factory.CompiledFunctionTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<CompiledFunctionType> {

		@Override
		public CompiledFunctionType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link CompiledFunctionType} type implementation.
		 */
		private static final class CompiledFunctionTypeImpl extends TypeBaseClass implements CompiledFunctionType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private CompiledFunctionTypeImpl() {
				super("COMPILED-FUNCTION");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof CompiledFunctionType);
			}
		}
	}
}

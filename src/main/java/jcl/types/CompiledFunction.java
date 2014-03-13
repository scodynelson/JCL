package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code CompiledFunction} is any function that contains no references to macros that must be expanded at run time,
 * and contains no unresolved references to load time values.
 * <p/>
 * {@code Function}s whose definitions appear lexically within a file that has been compiled and then loaded are of type
 * {@code CompiledFunction}.
 * <p/>
 * {@code CompiledFunction} -> {@code Function} -> {@code T}
 */
public interface CompiledFunction extends Function {

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
		 * Inner {@code CompiledFunction} type implementation.
		 */
		private static class CompiledFunctionImpl implements CompiledFunction, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof CompiledFunction);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

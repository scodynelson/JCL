package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link CompiledFunction} is any function that contains no references to macros that must be expanded at run time,
 * and contains no unresolved references to load time values.
 * <p>
 * {@link Function}s whose definitions appear lexically within a file that has been compiled and then loaded are of type
 * {@link CompiledFunction}.
 * <p>
 * {@link CompiledFunction} -> {@link Function} -> {@link T}
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
		 * Inner {@link CompiledFunction} type implementation.
		 */
		private static class CompiledFunctionImpl extends TypeBaseClass implements CompiledFunction, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private CompiledFunctionImpl() {
				super("COMPILED-FUNCTION", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof CompiledFunction);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "CompiledFunctionImpl{}";
			}
		}
	}
}

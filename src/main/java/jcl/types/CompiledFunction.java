package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link CompiledFunction} is any function that contains no references to macros that must be expanded at run time,
 * and contains no unresolved references to load time values.
 * <p>
 * {@link Function}s whose definitions appear lexically within a file that has been compiled and then loaded are of
 * type {@link CompiledFunction}.
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
		private static final class CompiledFunctionImpl extends TypeBaseClass implements CompiledFunction, AtomicTypeSpecifier {

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
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

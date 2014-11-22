package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * The class {@link StandardGenericFunction} is the default {@link Class} of {@link GenericFunction}s.
 * <p>
 * {@link StandardGenericFunction} -> {@link GenericFunction} -> {@link Function} -> {@link T}
 */
public interface StandardGenericFunction extends GenericFunction {

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
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

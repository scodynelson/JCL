package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link MethodCombination} object represents the information about the {@link MethodCombination} being used by a
 * {@link GenericFunction}. A {@link MethodCombination} object contains information about both the type of
 * {@link MethodCombination} and the arguments being used with that type.
 * <p>
 * {@link MethodCombination} -> {@link T}
 */
public interface MethodCombination extends T {

	MethodCombination INSTANCE = new Factory.MethodCombinationImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<MethodCombination> {

		@Override
		public MethodCombination getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link MethodCombination} type implementation.
		 */
		private static final class MethodCombinationImpl extends TypeBaseClass implements MethodCombination, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private MethodCombinationImpl() {
				super("METHOD-COMBINATION");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof MethodCombination);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link RandomState} object contains state information used by the pseudo-random number generator.
 * <p>
 * {@link RandomState} -> {@link T}
 */
public interface RandomState extends T {

	RandomState INSTANCE = new Factory.RandomStateImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<RandomState> {

		@Override
		public RandomState getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link RandomState} type implementation.
		 */
		private static final class RandomStateImpl extends TypeBaseClass implements RandomState, AtomicTypeSpecifier {

			private static final long serialVersionUID = -8325573429698690966L;

			/**
			 * Private constructor.
			 */
			private RandomStateImpl() {
				super("RANDOM-STATE");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof RandomState);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

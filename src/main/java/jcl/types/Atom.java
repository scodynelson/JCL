package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.NotTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * An {@link Atom} is a type equivalent to (not cons).
 * <p>
 * {@link Atom} -> {@link T}
 */
public interface Atom extends T {

	Atom INSTANCE = new Factory.AtomImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Atom> {

		@Override
		public Atom getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Atom} type implementation.
		 */
		private static class AtomImpl extends NotTypeSpecifier implements Atom, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private AtomImpl() {
				super("ATOM", Cons.INSTANCE);
			}

			@Override
			public boolean equals(final Object obj) {
				return super.equals(obj) || (obj == INSTANCE);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

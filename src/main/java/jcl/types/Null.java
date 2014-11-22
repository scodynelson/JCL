package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * The only object of type {@link Null} is {@link NIL}, which represents the empty {@link List} and can also be notated
 * ().
 * <p>
 * {@link Null} -> {@link Symbol} -> {@link List} -> {@link Sequence} -> {@link T}
 */
public interface Null extends Symbol, List {

	Null INSTANCE = new Factory.NullImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Null> {

		@Override
		public Null getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Null} type implementation.
		 */
		private static final class NullImpl extends TypeBaseClass implements Null, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private NullImpl() {
				super("NULL");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Null) || (obj instanceof NIL);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link List} is a chain of {@link Cons}es in which the car of each {@link Cons} is an element of the {@link List},
 * and the cdr of each {@link Cons} is either the next link in the chain or a terminating {@link Atom}.
 * <p>
 * A proper {@link List} is a chain of {@link Cons}es terminated by the empty {@link List}, (), which is itself a
 * proper {@link List}. A dotted {@link List} is a {@link List} which has a terminating {@link Atom} that is not the
 * empty {@link List}. A circular {@link List} is a chain of {@link Cons}es that has no termination because some {@link
 * Cons} in the chain is the cdr of a later {@link Cons}.
 * <p>
 * Dotted {@link List}s and circular {@link List}s are also {@link List}s, but usually the unqualified term ''list''
 * within this specification means proper {@link List}. Nevertheless, the type {@link List} unambiguously includes
 * dotted {@link List}s and circular {@link List}s.
 * <p>
 * For each element of a {@link List} there is a {@link Cons}. The empty {@link List} has no elements and is not a
 * {@link Cons}.
 * <p>
 * The types {@link Cons} and {@link Null} form an exhaustive partition of the type {@link List}.
 * <p>
 * {@link List} -> {@link Sequence} -> {@link T}
 */
public interface List extends Sequence {

	List INSTANCE = new Factory.ListImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<List> {

		@Override
		public List getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link List} type implementation.
		 */
		private static final class ListImpl extends TypeBaseClass implements List, AtomicTypeSpecifier {

			private static final long serialVersionUID = 8480820578792501223L;

			/**
			 * Private constructor.
			 */
			private ListImpl() {
				super("LIST");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof List);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}

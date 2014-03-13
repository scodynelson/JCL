package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code List} is a chain of {@code Cons}es in which the car of each {@code Cons} is an element of the {@code List},
 * and the cdr of each {@code Cons} is either the next link in the chain or a terminating {@code Atom}.
 * <p/>
 * A proper {@code List} is a chain of {@code Cons}es terminated by the empty {@code List}, (), which is itself a proper
 * {@code List}. A dotted {@code List} is a {@code List} which has a terminating {@code Atom} that is not the empty
 * {@code List}. A circular {@code List} is a chain of {@code Cons}es that has no termination because some {@code Cons}
 * in the chain is the cdr of a later {@code Cons}.
 * <p/>
 * Dotted {@code List}s and circular {@code List}s are also {@code List}s, but usually the unqualified term ''list''
 * within this specification means proper {@code List}. Nevertheless, the type {@code List} unambiguously includes
 * dotted {@code List}s and circular {@code List}s.
 * <p/>
 * For each element of a {@code List} there is a {@code Cons}. The empty {@code List} has no elements and is not a
 * {@code Cons}.
 * <p/>
 * The types {@code Cons} and {@code Null} form an exhaustive partition of the type {@code List}.
 * <p/>
 * {@code List} -> {@code Sequence} -> {@code T}
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
		 * Inner {@code List} type implementation.
		 */
		private static class ListImpl implements List, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof List);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

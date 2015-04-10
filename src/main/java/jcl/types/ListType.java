/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link ListType} is a chain of {@link ConsType}es in which the car of each {@link ConsType} is an element of the
 * {@link ListType}, and the cdr of each {@link ConsType} is either the next link in the chain or a terminating {@link
 * AtomType}.
 * <p>
 * A proper {@link ListType} is a chain of {@link ConsType}es terminated by the empty {@link ListType}, (), which is
 * itself a proper {@link ListType}. A dotted {@link ListType} is a {@link ListType} which has a terminating {@link
 * AtomType} that is not the empty {@link ListType}. A circular {@link ListType} is a chain of {@link ConsType}es that
 * has no termination because some {@link ConsType} in the chain is the cdr of a later {@link ConsType}.
 * <p>
 * Dotted {@link ListType}s and circular {@link ListType}s are also {@link ListType}s, but usually the unqualified term
 * ''list'' within this specification means proper {@link ListType}. Nevertheless, the type {@link ListType}
 * unambiguously includes dotted {@link ListType}s and circular {@link ListType}s.
 * <p>
 * For each element of a {@link ListType} there is a {@link ConsType}. The empty {@link ListType} has no elements and
 * is not a {@link ConsType}.
 * <p>
 * The types {@link ConsType} and {@link NullType} form an exhaustive partition of the type {@link ListType}.
 * <p>
 * {@link ListType} -&gt; {@link SequenceType} -&gt; {@link TType}
 */
public interface ListType extends SequenceType {

	/**
	 * Singleton instance of the {@link ListType} type.
	 */
	ListType INSTANCE = new Factory.ListTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ListType> {

		@Override
		public ListType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ListType} type implementation.
		 */
		private static final class ListTypeImpl extends TypeBaseClass implements ListType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 8480820578792501223L;

			/**
			 * Private constructor.
			 */
			private ListTypeImpl() {
				super("LIST");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof ListType);
			}
		}
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link HashTable} provide a way of mapping any object (a key) to an associated object (a value).
 * <p>
 * {@link HashTable} -&gt; {@link T}
 */
public interface HashTable extends T {

	/**
	 * Singleton instance of the {@link HashTable} type.
	 */
	HashTable INSTANCE = new Factory.HashTableImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<HashTable> {

		@Override
		public HashTable getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link HashTable} type implementation.
		 */
		private static final class HashTableImpl extends TypeBaseClass implements HashTable, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 1398788699336001310L;

			/**
			 * Private constructor.
			 */
			private HashTableImpl() {
				super("HASH-TABLE");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof HashTable);
			}

			@Override
			public String toString() {
//				return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
				return getName();
			}
		}
	}
}

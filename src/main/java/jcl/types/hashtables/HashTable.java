package jcl.types.hashtables;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code HashTable} provide a way of mapping any object (a key) to an associated object (a value).
 * <p/>
 * {@code HashTable} -> {@code T}
 */
public interface HashTable extends T {

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
		 * Inner {@code HashTable} type implementation.
		 */
		private static class HashTableImpl implements HashTable, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof HashTable);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

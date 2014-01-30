package jcl.types.hashtables;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code HashTable} provide a way of mapping any object (a key) to an associated object (a value).
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
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof HashTable)) {
					return false;
				}

				final HashTable hashTable = (HashTable) obj;
				return hashTable == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}

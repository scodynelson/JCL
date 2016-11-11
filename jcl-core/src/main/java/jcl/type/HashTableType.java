/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link HashTableType} provide a way of mapping any object (a key) to an associated object (a value).
 * <p>
 * {@link HashTableType} -&gt; {@link TType}
 */
public interface HashTableType extends TType {

	/**
	 * Singleton instance of the {@link HashTableType} type.
	 */
	HashTableType INSTANCE = new Factory.HashTableTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<HashTableType> {

		@Override
		public HashTableType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link HashTableType} type implementation.
		 */
		private static final class HashTableTypeImpl extends TypeBaseClass implements HashTableType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private HashTableTypeImpl() {
				super("HASH-TABLE");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof HashTableType);
			}
		}
	}
}

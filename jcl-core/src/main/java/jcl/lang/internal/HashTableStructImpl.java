/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.HashTableStruct;
import jcl.lang.LispStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.type.HashTableType;
import jcl.type.LispType;

/**
 * The {@link HashTableStructImpl} is the object representation of a Lisp 'hash-table' type.
 * <p>
 * NOTE: This implementation does NOT support size tracking or rehash-size customization. These are handled internally
 * by Java.
 */
public final class HashTableStructImpl extends BuiltInClassStruct implements HashTableStruct {

	/**
	 * The test function for verifying equivalence of a key.
	 */
	private final FunctionStruct test;

	/**
	 * The threshold used in the rehashing of the {@link #map}.
	 */
	private final float rehashThreshold;

	/**
	 * The internal {@link Map} containing the {@link LispStruct} keys and values.
	 */
	private final Map<LispStruct, LispStruct> map;

	/**
	 * Public constructor.
	 *
	 * @param test
	 * 		the test function for determining key matching
	 * @param size
	 * 		the initial size of the table
	 * @param rehashThreshold
	 * 		the threshold amount when resizing the table
	 */
	private HashTableStructImpl(final FunctionStruct test, final BigInteger size, final float rehashThreshold) {
		super(HashTableType.INSTANCE, null, null);
		this.test = test;
		this.rehashThreshold = rehashThreshold;

		map = new ConcurrentHashMap<>(size.intValue(), rehashThreshold);
	}

	public static HashTableStructImpl valueOf(final FunctionStruct test, final BigInteger size, final float rehashThreshold) {
		return new HashTableStructImpl(test, size, rehashThreshold);
	}

	/**
	 * Getter for hash-table {@link #test} property.
	 *
	 * @return hash-table {@link #test} property
	 */
	@Override
	public FunctionStruct getTest() {
		return test;
	}

	/**
	 * Returns {@link BigDecimal#ONE} for the hash-table rehash size.
	 *
	 * @return {@link BigDecimal#ONE} for the hash-table rehash size
	 */
	@Override
	public BigDecimal getRehashSize() {
		return BigDecimal.ONE;
	}

	/**
	 * Getter for hash-table {@link #rehashThreshold} property.
	 *
	 * @return hash-table {@link #rehashThreshold} property
	 */
	@Override
	public float getRehashThreshold() {
		return rehashThreshold;
	}

	/**
	 * Gets the current size of the internal map.
	 *
	 * @return the current size of the internal map
	 */
	@Override
	public BigInteger getSize() {
		return getCount();
	}

	/**
	 * Gets the current number of items in the internal map.
	 *
	 * @return the current number of items in the internal map
	 */
	@Override
	public BigInteger getCount() {
		return BigInteger.valueOf(map.size());
	}

	/**
	 * Returns the value stored in the map matching the provided key.
	 *
	 * @param key
	 * 		the key to find the matching stored value
	 *
	 * @return the matching stored value for the provided key
	 */
	@Override
	public LispStruct getHash(final LispStruct key) {
		final LispStruct keyWrapper = KeyWrapper.getInstance(key, test);
		return map.get(keyWrapper);
	}

	/**
	 * Sets or inserts the value stored in the map matching the provided key.
	 *
	 * @param key
	 * 		the key to set or insert the provided value
	 * @param value
	 * 		the value to be stored in the table
	 */
	@Override
	public void setHash(final LispStruct key, final LispStruct value) {
		final LispStruct keyWrapper = KeyWrapper.getInstance(key, test);
		map.put(keyWrapper, value);
	}

	/**
	 * Removes the value stored in the map matching the provided key.
	 *
	 * @param key
	 * 		the key to remove the matching stored value
	 *
	 * @return the removed value or {@code null} if no value existed
	 */
	@Override
	public LispStruct remHash(final LispStruct key) {
		final LispStruct keyWrapper = KeyWrapper.getInstance(key, test);
		return map.remove(keyWrapper);
	}

	/**
	 * Clears the internal map.
	 */
	@Override
	public void clrHash() {
		map.clear();
	}

	/**
	 * Runs a mapping function over the internal map.
	 *
	 * @param function
	 * 		the mapping function
	 */
	@Override
	public void mapHash(final FunctionStruct function) {
		for (final Map.Entry<LispStruct, LispStruct> entry : map.entrySet()) {
			final LispStruct keyWrapper = new KeyWrapper(entry.getKey(), test);
			function.apply(keyWrapper, entry.getValue());
		}
	}

	@Override
	public boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof HashTableStructImpl) {
			final HashTableStructImpl ht = (HashTableStructImpl) object;
			if (map.size() != ht.map.size()) {
				return false;
			}
			if (!test.eq(ht.test)) {
				return false;
			}
			for (final Map.Entry<LispStruct, LispStruct> entry : map.entrySet()) {
				final LispStruct key = entry.getKey();
				final LispStruct value = entry.getValue();
				final LispStruct objectValue = ht.map.get(key);

				if (!value.equalp(objectValue)) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	@Override
	public String toString() {
//		final String typeClassName = getType().getClass().getSimpleName().toUpperCase();

		final String printedTest = test.toString();

		final BigInteger mapSize = getCount();

		return "#<" + "HASH-TABLE" + " :TEST " + printedTest + " :SIZE " + mapSize + '>';
	}

	/**
	 * Private inner class that acts as a wrapper around hash keys for proper equality testing.
	 */
	private static final class KeyWrapper implements LispStruct {

		/**
		 * The {@link LispStruct} key to wrap.
		 */
		private final LispStruct key;

		/**
		 * The {@link FunctionStruct} used to test equivalence of a key.
		 */
		private final FunctionStruct equivalenceFn;

		/**
		 * Private constructor.
		 *
		 * @param key
		 * 		the key to wrap
		 * @param equivalenceFn
		 * 		the equivalence function used to test equality of keys
		 */
		private KeyWrapper(final LispStruct key, final FunctionStruct equivalenceFn) {
			this.key = key;
			this.equivalenceFn = equivalenceFn;
		}

		/**
		 * Getter for key-wrapper {@link #key} property.
		 *
		 * @return key-wrapper {@link #key} property
		 */
		public LispStruct getKey() {
			return key;
		}

		@Override
		public LispType getType() {
			return key.getType();
		}

		@Override
		public int hashCode() {
			return key.hashCode();
		}

		@Override
		public boolean equals(final Object obj) {
			if (!(obj instanceof LispStruct)) {
				return false;
			}

			final LispStruct lispStruct = (LispStruct) obj;
			return ((BooleanStruct) equivalenceFn.apply(key, lispStruct)).toJavaPBoolean();
		}

		@Override
		public String toString() {
			return key.toString();
		}

		/**
		 * Gets instance of KeyWrapper object.
		 *
		 * @param key
		 * 		the key to wrap
		 * @param equator
		 * 		the equator function used to test equality of keys
		 *
		 * @return the newly created KeyWrapper object
		 */
		private static KeyWrapper getInstance(final LispStruct key, final FunctionStruct equator) {
			return new KeyWrapper(key, equator);
		}
	}
}

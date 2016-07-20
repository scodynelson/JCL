/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.hashtable;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.BuiltInClassStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.EquatorFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.type.HashTableType;
import jcl.type.LispType;

/**
 * The {@link HashTableStruct} is the object representation of a Lisp 'hash-table' type.
 * <p>
 * NOTE: This implementation does NOT support size tracking or rehash-size customization. These are handled internally
 * by Java.
 */
public class HashTableStruct extends BuiltInClassStruct {

	/**
	 * The test function for verifying equivalence of a key.
	 */
	private final EquatorFunctionStruct test;

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
	public HashTableStruct(final EquatorFunctionStruct test, final BigInteger size, final float rehashThreshold) {
		super(HashTableType.INSTANCE, null, null);
		this.test = test;
		this.rehashThreshold = rehashThreshold;

		map = new ConcurrentHashMap<>(size.intValue(), rehashThreshold);
	}

	/**
	 * Getter for hash-table {@link #test} property.
	 *
	 * @return hash-table {@link #test} property
	 */
	public FunctionStruct getTest() {
		return test;
	}

	/**
	 * Returns {@link BigDecimal#ONE} for the hash-table rehash size.
	 *
	 * @return {@link BigDecimal#ONE} for the hash-table rehash size
	 */
	public static BigDecimal getRehashSize() {
		return BigDecimal.ONE;
	}

	/**
	 * Getter for hash-table {@link #rehashThreshold} property.
	 *
	 * @return hash-table {@link #rehashThreshold} property
	 */
	public float getRehashThreshold() {
		return rehashThreshold;
	}

	/**
	 * Gets the current size of the internal map.
	 *
	 * @return the current size of the internal map
	 */
	public BigInteger getSize() {
		return getCount();
	}

	/**
	 * Gets the current number of items in the internal map.
	 *
	 * @return the current number of items in the internal map
	 */
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
	public LispStruct remHash(final LispStruct key) {
		final LispStruct keyWrapper = KeyWrapper.getInstance(key, test);
		return map.remove(keyWrapper);
	}

	/**
	 * Clears the internal map.
	 */
	public void clrHash() {
		map.clear();
	}

	/**
	 * Runs a mapping function over the internal map.
	 *
	 * @param function
	 * 		the mapping function
	 */
	public void mapHash(final FunctionStruct function) {
		for (final Map.Entry<LispStruct, LispStruct> entry : map.entrySet()) {
			final LispStruct keyWrapper = KeyWrapper.getInstance(entry.getKey(), test);
			function.apply(keyWrapper, entry.getValue());
		}
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
	public static final class KeyWrapper implements LispStruct {

		/**
		 * The {@link LispStruct} key to wrap.
		 */
		private final LispStruct key;

		/**
		 * The {@link EquatorFunctionStruct} used to test equivalence of a key.
		 */
		private final EquatorFunctionStruct equator;

		/**
		 * Private constructor.
		 *
		 * @param key
		 * 		the key to wrap
		 * @param equator
		 * 		the equator function used to test equality of keys
		 */
		private KeyWrapper(final LispStruct key, final EquatorFunctionStruct equator) {
			this.key = key;
			this.equator = equator;
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
			return equator.hash(key);
		}

		@Override
		public boolean equals(final Object obj) {
			if (!(obj instanceof LispStruct)) {
				return false;
			}

			final LispStruct lispStruct = (LispStruct) obj;
			return equator.equate(key, lispStruct);
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
		private static KeyWrapper getInstance(final LispStruct key, final EquatorFunctionStruct equator) {
			return new KeyWrapper(key, equator);
		}
	}
}
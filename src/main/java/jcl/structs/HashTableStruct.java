package jcl.structs;

import jcl.LispStruct;
import jcl.functions.EquatorFunctionStruct;
import jcl.types.HashTable;
import jcl.LispType;
import org.apache.commons.collections4.Equator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The {@code HashTableStruct} is the object representation of a Lisp 'hash-table' type.
 * <p/>
 * NOTE: This implementation does NOT support size tracking or rehash-size customization. These are handled internally by Java.
 */
public class HashTableStruct extends BuiltInClassStruct {

	private final EquatorFunctionStruct<LispStruct> test;
	private final BigDecimal rehashThreshold;

	private final Map<LispStruct, LispStruct> map;

	/**
	 * Public constructor.
	 *
	 * @param test            the test function for determining key matching
	 * @param size            the initial size of the table
	 * @param rehashThreshold the threshold amount when resizing the table
	 */
	public HashTableStruct(final EquatorFunctionStruct<LispStruct> test, final BigInteger size, final BigDecimal rehashThreshold) {
		super(HashTable.INSTANCE, null, null);
		this.test = test;
		this.rehashThreshold = rehashThreshold;

		map = new ConcurrentHashMap<>(size.intValue(), rehashThreshold.floatValue());
	}

	/**
	 * Getter for hash-table test property.
	 *
	 * @return hash-table test property
	 */
	public FunctionStruct getTest() {
		return test;
	}

	/**
	 * Getter for hash-table rehashThreshold property.
	 *
	 * @return hash-table rehashThreshold property
	 */
	public BigDecimal getRehashThreshold() {
		return rehashThreshold;
	}

	/**
	 * This method gets the current number of items in the internal map.
	 *
	 * @return the current number of items in the internal map
	 */
	public BigInteger getCount() {
		return BigInteger.valueOf(map.size());
	}

	/**
	 * This method returns the value stored in the map matching the provided key.
	 *
	 * @param key the key to find the matching stored value
	 * @return the matching stored value for the provided key
	 */
	public LispStruct getHash(final LispStruct key) {
		final LispStruct keyWrapper = KeyWrapper.getInstance(key, test);
		return map.get(keyWrapper);
	}

	/**
	 * This method sets or inserts the value stored in the map matching the provided key.
	 *
	 * @param key   the key to set or insert the provided value
	 * @param value the value to be stored in the table
	 */
	public void setHash(final LispStruct key, final LispStruct value) {
		final LispStruct keyWrapper = KeyWrapper.getInstance(key, test);
		map.put(keyWrapper, value);
	}

	/**
	 * This method removes the value stored in the map matching the provided key.
	 *
	 * @param key the key to remove the matching stored value
	 */
	public void remHash(final LispStruct key) {
		final LispStruct keyWrapper = KeyWrapper.getInstance(key, test);
		map.remove(keyWrapper);
	}

	/**
	 * This method clears the internal map.
	 */
	public void clrHash() {
		map.clear();
	}

	/**
	 * This method runs a mapping function over the internal map.
	 *
	 * @param function the mapping function
	 */
	public void mapHash(final FunctionStruct function) {
		for (final Map.Entry<LispStruct, LispStruct> entry : map.entrySet()) {
			final LispStruct keyWrapper = KeyWrapper.getInstance(entry.getKey(), test);
			function.apply(keyWrapper, entry.getValue());
		}
	}

	@Override
	public String toString() {
		return "HashTableStruct{"
				+ "test=" + test
				+ ", rehashThreshold=" + rehashThreshold
				+ ", map=" + map
				+ '}';
	}

	/**
	 * Private inner class that acts as a wrapper around hash keys for proper equality testing.
	 */
	private static class KeyWrapper implements LispStruct {

		private final LispStruct key;
		private final Equator<LispStruct> equator;

		/**
		 * Private constructor.
		 *
		 * @param key     the key to wrap
		 * @param equator the equator function used to test equality of keys
		 */
		private KeyWrapper(final LispStruct key, final Equator<LispStruct> equator) {
			this.key = key;
			this.equator = equator;
		}

		@Override
		public LispType getType() {
			return key.getType();
		}

		/**
		 * Gets instance of {@code KeyWrapper} object.
		 *
		 * @param key     the key to wrap
		 * @param equator the equator function used to test equality of keys
		 * @return the newly created {@code KeyWrapper} object
		 */
		public static KeyWrapper getInstance(final LispStruct key, final Equator<LispStruct> equator) {
			return new KeyWrapper(key, equator);
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
		public int hashCode() {
			return new HashCodeBuilder()
					.append(key)
					.append(equator)
					.toHashCode();
		}

		@Override
		public String toString() {
			return "KeyWrapper{"
					+ "key=" + key
					+ "equator=" + equator
					+ '}';
		}
	}
}

package jcl.lang;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.lang.function.FunctionStruct;

/**
 * The {@link HashTableStruct} is the object representation of a Lisp 'hash-table' type.
 */
public interface HashTableStruct extends LispStruct {

	/**
	 * Getter for hash-table test property.
	 *
	 * @return hash-table test property
	 */
	FunctionStruct getTest();

	/**
	 * Getter for hash-table rehash-size property.
	 *
	 * @return hash-table rehash-size property
	 */
	BigDecimal getRehashSize();

	/**
	 * Getter for hash-table rehash-threshold property.
	 *
	 * @return hash-table rehash-threshold property
	 */
	float getRehashThreshold();

	/**
	 * Gets the current size of the internal map.
	 *
	 * @return the current size of the internal map
	 */
	BigInteger getSize();

	/**
	 * Gets the current number of items in the internal map.
	 *
	 * @return the current number of items in the internal map
	 */
	BigInteger getCount();

	/**
	 * Returns the value stored in the map matching the provided key.
	 *
	 * @param key
	 * 		the key to find the matching stored value
	 *
	 * @return the matching stored value for the provided key
	 */
	LispStruct getHash(final LispStruct key);

	/**
	 * Sets or inserts the value stored in the map matching the provided key.
	 *
	 * @param key
	 * 		the key to set or insert the provided value
	 * @param value
	 * 		the value to be stored in the table
	 */
	void setHash(final LispStruct key, final LispStruct value);

	/**
	 * Removes the value stored in the map matching the provided key.
	 *
	 * @param key
	 * 		the key to remove the matching stored value
	 *
	 * @return the removed value or {@code null} if no value existed
	 */
	LispStruct remHash(final LispStruct key);

	/**
	 * Clears the internal map.
	 */
	void clrHash();

	/**
	 * Runs a mapping function over the internal map.
	 *
	 * @param function
	 * 		the mapping function
	 */
	void mapHash(final FunctionStruct function);
}

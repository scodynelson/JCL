package jcl.lang;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.HashTableStructImpl;

/**
 * The {@link HashTableStruct} is the object representation of a Lisp 'hash-table' type.
 */
public interface HashTableStruct extends LispStruct {

	/**
	 * Returns all hash-table entries.
	 *
	 * @return all hash-table entries
	 */
	ListStruct entries();

	/**
	 * Getter for hash-table test property.
	 *
	 * @return hash-table test property
	 */
	FunctionStruct test();

	/**
	 * Getter for hash-table rehash-size property.
	 *
	 * @return hash-table rehash-size property
	 */
	FloatStruct rehashSize();

	/**
	 * Getter for hash-table rehash-threshold property.
	 *
	 * @return hash-table rehash-threshold property
	 */
	FloatStruct rehashThreshold();

	/**
	 * Gets the current size of the internal map.
	 *
	 * @return the current size of the internal map
	 */
	IntegerStruct size();

	/**
	 * Gets the current number of items in the internal map.
	 *
	 * @return the current number of items in the internal map
	 */
	IntegerStruct count();

	/**
	 * Returns the value stored in the map matching the provided key.
	 *
	 * @param key
	 * 		the key to find the matching stored value
	 *
	 * @return the matching stored value for the provided key
	 */
	default GetHashResult getHash(final LispStruct key) {
		return getHash(key, NILStruct.INSTANCE);
	}

	/**
	 * Returns the value stored in the map matching the provided key.
	 *
	 * @param key
	 * 		the key to find the matching stored value
	 * @param defaultValue
	 * 		the default value to be returned if no mapping matches the provided key
	 *
	 * @return the matching stored value for the provided key
	 */
	GetHashResult getHash(final LispStruct key, final LispStruct defaultValue);

	/**
	 * Sets or inserts the value stored in the map matching the provided key.
	 *
	 * @param key
	 * 		the key to set or insert the provided value
	 * @param value
	 * 		the value to be stored in the table
	 *
	 * @return the stored value
	 */
	LispStruct putHash(final LispStruct key, final LispStruct value);

	/**
	 * Removes the value stored in the map matching the provided key.
	 *
	 * @param key
	 * 		the key to remove the matching stored value
	 *
	 * @return the removed value or {@code null} if no value existed
	 */
	BooleanStruct remHash(final LispStruct key);

	/**
	 * Clears the internal map.
	 *
	 * @return the instance with the internal map cleared
	 */
	HashTableStruct clrHash();

	/**
	 * Runs a mapping function over the internal map.
	 *
	 * @param designator
	 * 		the mapping function
	 *
	 * @return NIL
	 */
	default LispStruct mapHash(final LispStruct designator) {
		final FunctionStruct functionVal;
		if (designator instanceof FunctionStruct) {
			functionVal = (FunctionStruct) designator;
		} else if (designator instanceof SymbolStruct) {
			functionVal = ((SymbolStruct) designator).symbolFunction();
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}

		return mapHashFunction(functionVal);
	}

	/**
	 * Runs a mapping function over the internal map.
	 *
	 * @param function
	 * 		the mapping function
	 *
	 * @return NIL
	 */
	LispStruct mapHashFunction(final FunctionStruct function);

	/**
	 * Returns the hash code from the provided object, which would be used when hashing a key in a hash-table.
	 *
	 * @param object
	 * 		the object to retrieve the hash code
	 *
	 * @return the hash code from the provided object
	 */
	static IntegerStruct sxHash(final LispStruct object) {
		return IntegerStruct.toLispInteger(object.hashCode());
	}

	/**
	 * Returns a new HashTableStruct with the provided {@link LispStruct} {@code test} to be used as the key comparator
	 * (should be a function-designator, initialized to the provided {@link FixnumStruct} {@code size}, with a rehashing
	 * threshold matching the provided {@link FloatStruct} {@code rehashThreshold}.
	 *
	 * @param test
	 * 		the {@link LispStruct} to be used as the key comparator; should be a function-designator
	 * @param size
	 * 		the initial size of the table
	 * @param rehashThreshold
	 * 		the rehashing threshold of the table
	 *
	 * @return a new HashTableStruct
	 */
	static HashTableStruct toLispHashTable(final LispStruct test, final FixnumStruct size,
	                                       final FloatStruct rehashThreshold) {
		final FunctionStruct realTest = FunctionStruct.toLispFunction(test);
		// TODO: check that test is one of: eq, eql, equal, equalp
		return new HashTableStructImpl(realTest, size, rehashThreshold);
	}
}

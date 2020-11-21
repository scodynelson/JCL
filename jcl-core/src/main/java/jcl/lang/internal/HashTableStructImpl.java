/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import jcl.lang.BooleanStruct;
import jcl.lang.ConsStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.FloatStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.HashTableStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SingleFloatStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link HashTableStructImpl} is the object representation of a Lisp 'hash-table' type.
 * <p>
 * NOTE: This implementation does NOT support size tracking or rehash-size customization. These are handled internally
 * by Java.
 */
public final class HashTableStructImpl extends LispStructImpl implements HashTableStruct {

	/**
	 * The test function for verifying equivalence of a key.
	 */
	private final FunctionStruct test;

	/**
	 * The threshold used in the rehashing of the {@link #map}.
	 */
	private final FloatStruct rehashThreshold;

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
	public HashTableStructImpl(final FunctionStruct test, final FixnumStruct size, final FloatStruct rehashThreshold) {
		this.test = test;
		this.rehashThreshold = rehashThreshold;

		map = new ConcurrentHashMap<>(size.toJavaInt(), rehashThreshold.toJavaPFloat());
	}

	@Override
	public ListStruct entries() {
		final List<ConsStruct> entriesList
				= map.entrySet()
				     .stream()
				     .map(entry -> ConsStruct.toLispCons(((KeyWrapper) entry.getKey()).getKey(),
				                                         entry.getValue()))
				     .collect(Collectors.toList());
		return ListStruct.toLispList(entriesList);
	}

	@Override
	public FunctionStruct test() {
		return test;
	}

	@Override
	public FloatStruct rehashSize() {
		return SingleFloatStruct.ONE;
	}

	@Override
	public FloatStruct rehashThreshold() {
		return rehashThreshold;
	}

	@Override
	public IntegerStruct size() {
		return count();
	}

	@Override
	public IntegerStruct count() {
		return IntegerStruct.toLispInteger(map.size());
	}

	@Override
	public LispStruct getHash(final LispStruct key, final LispStruct defaultValue) {
		final LispStruct keyWrapper = KeyWrapper.getInstance(key, test);
		if (map.containsKey(keyWrapper)) {
			return ValuesStruct.valueOf(map.get(keyWrapper), TStruct.INSTANCE);
		} else {
			return ValuesStruct.valueOf(defaultValue, NILStruct.INSTANCE);
		}
	}

	@Override
	public LispStruct putHash(final LispStruct key, final LispStruct value) {
		final LispStruct keyWrapper = KeyWrapper.getInstance(key, test);
		map.put(keyWrapper, value);
		return value;
	}

	@Override
	public BooleanStruct remHash(final LispStruct key) {
		final LispStruct keyWrapper = KeyWrapper.getInstance(key, test);
		if (map.containsKey(keyWrapper)) {
			map.remove(keyWrapper);
			return TStruct.INSTANCE;
		} else {
			return NILStruct.INSTANCE;
		}
	}

	@Override
	public HashTableStruct clrHash() {
		map.clear();
		return this;
	}

	@Override
	public LispStruct mapHash(final FunctionStruct function) {
		for (final Map.Entry<LispStruct, LispStruct> entry : map.entrySet()) {
			final LispStruct keyWrapper = new KeyWrapper(entry.getKey(), test);
			function.apply(keyWrapper, entry.getValue());
		}
		return NILStruct.INSTANCE;
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
	public LispStruct typeOf() {
		return CommonLispSymbols.HASH_TABLE;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.HASH_TABLE;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.HASH_TABLE) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.HASH_TABLE) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}

	@Override
	public String toString() {
		return "#<" + "HASH-TABLE" + " :TEST " + test + " :SIZE " + size() + '>';
	}

	/**
	 * Private inner class that acts as a wrapper around hash keys for proper equality testing.
	 */
	private static final class KeyWrapper extends LispStructImpl {

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
		public LispStruct typeOf() {
			return key.typeOf();
		}

		@Override
		public int hashCode() {
			return key.hashCode();
		}

		@Override
		public boolean equals(final Object obj) {
			if (!(obj instanceof KeyWrapper)) {
				return false;
			}

			final KeyWrapper lispStruct = (KeyWrapper) obj;
			return ((BooleanStruct) equivalenceFn.apply(key, lispStruct.key)).toJavaPBoolean();
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

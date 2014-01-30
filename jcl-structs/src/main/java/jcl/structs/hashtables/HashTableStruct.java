package jcl.structs.hashtables;

import jcl.structs.LispStruct;
import jcl.structs.functions.FunctionStruct;
import jcl.types.LispType;
import jcl.types.hashtables.HashTable;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;
import java.util.ResizableHashMap;

public class HashTableStruct implements LispStruct {

	private final FunctionStruct test;
	private final BigInteger size;
	private final BigInteger rehashSize;
	private final BigDecimal rehashThreshold;

	private final Map<LispStruct, LispStruct> map;

	public HashTableStruct(final FunctionStruct test, final BigInteger size, final BigInteger rehashSize,
						   final BigDecimal rehashThreshold) {
		this.test = test;
		this.size = size;
		this.rehashSize = rehashSize;
		this.rehashThreshold = rehashThreshold;

		map = new ResizableHashMap<>(size.intValue(), rehashSize.floatValue(), rehashSize.intValue());
	}

	@Override
	public LispType getType() {
		return HashTable.INSTANCE;
	}

	public FunctionStruct getTest() {
		return test;
	}

	public BigInteger getSize() {
		// TODO: this will need to be updated every time a rehashing happens.
		return size;
	}

	public BigInteger getRehashSize() {
		return rehashSize;
	}

	public BigDecimal getRehashThreshold() {
		return rehashThreshold;
	}

	public BigInteger getCount() {
		return BigInteger.valueOf(map.size());
	}

	public LispStruct getHash(final LispStruct key) {
		return map.get(key);
	}

	public void setHash(final LispStruct key, final LispStruct value) {
		map.put(key, value);
	}

	public void remHash(final LispStruct key) {
		map.remove(key);
	}

	public void clrHash() {
		map.clear();
	}

	public void mapHash(final FunctionStruct function) {
		// TODO: do this...
	}

	// BUILDERS

	public static HashTableStruct getStruct(final FunctionStruct test, final BigInteger size, final BigInteger rehashSize, final BigDecimal rehashThreshold) {
		return new HashTableStruct(test, size, rehashSize, rehashThreshold);
	}
}

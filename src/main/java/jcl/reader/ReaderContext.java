/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;

public final class ReaderContext {

	/**
	 * Map containing the number argument to #= to parsed {@link LispStruct}s produced by the #= reader macro function.
	 */
	private final Map<Integer, LispStruct> sharpEqualFinalTable = new ConcurrentHashMap<>();

	/**
	 * Map containing the number argument of #= to a temporary {@link UUID} tag value to handle {@link LispStruct}s not
	 * yet parsed by the reader.
	 */
	private final Map<Integer, SymbolStruct> sharpEqualTempTable = new ConcurrentHashMap<>();

	/**
	 * Map containing the temporary {@link UUID} tag value to a {@link LispStruct} that has been parsed by the reader,
	 * but may have yet to return to the top level of the #= parse.
	 */
	private final Map<SymbolStruct, LispStruct> sharpEqualReplTable = new ConcurrentHashMap<>();

	/**
	 * The current backquote nesting level when dealing with nested backquotes. This is used to determine proper comma
	 * usage and evaluation.
	 */
	private int backquoteLevel;

	public Map<Integer, LispStruct> getSharpEqualFinalTable() {
		return new HashMap<>(sharpEqualFinalTable);
	}

	public Map<Integer, SymbolStruct> getSharpEqualTempTable() {
		return new HashMap<>(sharpEqualTempTable);
	}

	public Map<SymbolStruct, LispStruct> getSharpEqualReplTable() {
		return new HashMap<>(sharpEqualReplTable);
	}

	public void clearSharpEqualTables() {
		sharpEqualFinalTable.clear();
		sharpEqualTempTable.clear();
		sharpEqualReplTable.clear();
	}

	public void restoreSharpEqualTables(final Map<Integer, LispStruct> newSharpEqualFinalTable,
	                                    final Map<Integer, SymbolStruct> newSharpEqualTempTable,
	                                    final Map<SymbolStruct, LispStruct> newSharpEqualReplTable) {
		clearSharpEqualTables();
		sharpEqualFinalTable.putAll(newSharpEqualFinalTable);
		sharpEqualTempTable.putAll(newSharpEqualTempTable);
		sharpEqualReplTable.putAll(newSharpEqualReplTable);
	}

	public int getBackquoteLevel() {
		return backquoteLevel;
	}

	public void incrementBackquoteLevel() {
		backquoteLevel++;
	}

	public void decrementBackquoteLevel() {
		backquoteLevel--;
	}
}

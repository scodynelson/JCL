/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStructImpl;
import jcl.lang.readtable.Reader;
import jcl.lang.InputStreamStruct;
import jcl.lang.stream.ReadPeekResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 * JCL Reader that handles reading in lisp tokens and parsing them as {@link LispStruct}s.
 */
@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
class ReaderImpl implements Reader {

	/**
	 * The {@link InputStreamStruct} the ReaderImpl reads lisp tokens from.
	 */
	private final InputStreamStruct inputStreamStruct;

	/**
	 * Map containing the number argument to #= to parsed {@link LispStruct}s produced by the #= reader macro function.
	 */
	private final Map<BigInteger, LispStruct> sharpEqualFinalTable = new ConcurrentHashMap<>();

	/**
	 * Map containing the number argument of #= to a temporary {@link UUID} tag value to handle {@link LispStruct}s not
	 * yet parsed by the reader.
	 */
	private final Map<BigInteger, SymbolStructImpl> sharpEqualTempTable = new ConcurrentHashMap<>();

	/**
	 * Map containing the temporary {@link UUID} tag value to a {@link LispStruct} that has been parsed by the reader,
	 * but may have yet to return to the top level of the #= parse.
	 */
	private final Map<SymbolStructImpl, LispStruct> sharpEqualReplTable = new ConcurrentHashMap<>();

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	/**
	 * The current backquote nesting level when dealing with nested backquotes. This is used to determine proper comma
	 * usage and evaluation.
	 */
	private int backquoteLevel;

	/**
	 * Public constructor for creating a new JCL Reader.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} used to read lisp tokens
	 */
	ReaderImpl(final InputStreamStruct inputStreamStruct) {
		this.inputStreamStruct = inputStreamStruct;
	}

	@Override
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final LispStruct token = readPreservingWhitespace(eofErrorP, eofValue, recursiveP);

		final ReadPeekResult possibleWhitespace = readChar(false, eofValue, false);
		final Integer codePoint = possibleWhitespace.getResult();
		if (!possibleWhitespace.isEof() && (!Character.isWhitespace(codePoint) || recursiveP)) {
			unreadChar(codePoint);
		}

		return token;
	}

	@Override
	public LispStruct readPreservingWhitespace(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (recursiveP) {
			final TokenBuilder tokenBuilder = new TokenBuilder(this, eofErrorP, eofValue);
			return readerStateMediator.read(tokenBuilder);
		}

		final Map<BigInteger, LispStruct> tempSharpEqualFinalTable = new HashMap<>(sharpEqualFinalTable);
		final Map<BigInteger, SymbolStructImpl> tempSharpEqualTempTable = new HashMap<>(sharpEqualTempTable);
		final Map<SymbolStructImpl, LispStruct> tempSharpEqualReplTable = new HashMap<>(sharpEqualReplTable);

		try {
			sharpEqualFinalTable.clear();
			sharpEqualTempTable.clear();
			sharpEqualReplTable.clear();

			return readPreservingWhitespace(eofErrorP, eofValue, true);
		} finally {
			// Clear them back out.
			sharpEqualFinalTable.clear();
			sharpEqualTempTable.clear();
			sharpEqualReplTable.clear();

			// Restore their values.
			sharpEqualFinalTable.putAll(tempSharpEqualFinalTable);
			sharpEqualTempTable.putAll(tempSharpEqualTempTable);
			sharpEqualReplTable.putAll(tempSharpEqualReplTable);
		}
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStreamStruct.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public void unreadChar(final int codePoint) {
		inputStreamStruct.unreadChar(codePoint);
	}

	@Override
	public InputStreamStruct getInputStreamStruct() {
		return inputStreamStruct;
	}

	@Override
	public Map<BigInteger, LispStruct> getSharpEqualFinalTable() {
		return sharpEqualFinalTable;
	}

	@Override
	public Map<BigInteger, SymbolStructImpl> getSharpEqualTempTable() {
		return sharpEqualTempTable;
	}

	@Override
	public Map<SymbolStructImpl, LispStruct> getSharpEqualReplTable() {
		return sharpEqualReplTable;
	}

	@Override
	public int getBackquoteLevel() {
		return backquoteLevel;
	}

	@Override
	public void incrementBackquoteLevel() {
		backquoteLevel++;
	}

	@Override
	public void decrementBackquoteLevel() {
		backquoteLevel--;
	}
}

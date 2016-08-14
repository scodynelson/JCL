/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import java.math.BigInteger;
import java.util.Map;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.stream.ReadPeekResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * JCL Reader that handles reading in lisp tokens and parsing them as {@link LispStruct}s.
 */
@Component
class ReaderImpl implements Reader {

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Override
	public LispStruct read(final ReaderInputStreamStruct inputStreamStruct, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final LispStruct token = readPreservingWhitespace(inputStreamStruct, eofErrorP, eofValue, recursiveP);

		final ReadPeekResult possibleWhitespace = readChar(inputStreamStruct, false, eofValue, false);
		final Integer codePoint = possibleWhitespace.getResult();
		if (!possibleWhitespace.isEof() && (!Character.isWhitespace(codePoint) || recursiveP)) {
			unreadChar(inputStreamStruct, codePoint);
		}

		return token;
	}

	@Override
	public LispStruct readPreservingWhitespace(final ReaderInputStreamStruct inputStreamStruct, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (recursiveP) {
			final TokenBuilder tokenBuilder = new TokenBuilder(inputStreamStruct, eofErrorP, eofValue);
			return readerStateMediator.read(tokenBuilder);
		}

		final Map<BigInteger, LispStruct> tempSharpEqualFinalTable = inputStreamStruct.getSharpEqualFinalTable();
		final Map<BigInteger, SymbolStruct> tempSharpEqualTempTable = inputStreamStruct.getSharpEqualTempTable();
		final Map<SymbolStruct, LispStruct> tempSharpEqualReplTable = inputStreamStruct.getSharpEqualReplTable();

		try {
			inputStreamStruct.clearSharpEqualTables();

			return readPreservingWhitespace(inputStreamStruct, eofErrorP, eofValue, true);
		} finally {
			inputStreamStruct.restoreSharpEqualTables(tempSharpEqualFinalTable, tempSharpEqualTempTable, tempSharpEqualReplTable);
		}
	}

	@Override
	public ReadPeekResult readChar(final ReaderInputStreamStruct inputStreamStruct, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final InputStreamStruct inputStream = inputStreamStruct.getInputStream();
		return inputStream.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public void unreadChar(final ReaderInputStreamStruct inputStreamStruct, final int codePoint) {
		final InputStreamStruct inputStream = inputStreamStruct.getInputStream();
		inputStream.unreadChar(codePoint);
	}
}

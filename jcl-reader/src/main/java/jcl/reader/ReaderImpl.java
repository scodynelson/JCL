/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import java.math.BigInteger;
import java.util.Map;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.stream.ReadPeekResult;
import jcl.reader.state.ReaderStateMediatorImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * JCL Reader that handles reading in lisp tokens and parsing them as {@link LispStruct}s.
 */
@Component
class ReaderImpl implements Reader {

	/**
	 * {@link ReaderStateMediatorImpl} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediatorImpl readerStateMediator;

	@Override
	public LispStruct read(final InputStreamStruct inputStreamStruct, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final LispStruct token = readPreservingWhitespace(inputStreamStruct, eofErrorP, eofValue, recursiveP);

		final ReadPeekResult possibleWhitespace = inputStreamStruct.readChar(false, eofValue, false);
		final Integer codePoint = possibleWhitespace.getResult();
		if (!possibleWhitespace.isEof() && (!Character.isWhitespace(codePoint) || recursiveP)) {
			inputStreamStruct.unreadChar(codePoint);
		}

		return token;
	}

	@Override
	public LispStruct readPreservingWhitespace(final InputStreamStruct inputStreamStruct, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (recursiveP) {
			final TokenBuilder tokenBuilder = new TokenBuilder(inputStreamStruct, eofErrorP, eofValue);
			return readerStateMediator.read(tokenBuilder);
		}

		final ReaderContext context = ReaderContextHolder.getContext();
		final Map<BigInteger, LispStruct> tempSharpEqualFinalTable = context.getSharpEqualFinalTable();
		final Map<BigInteger, SymbolStruct> tempSharpEqualTempTable = context.getSharpEqualTempTable();
		final Map<SymbolStruct, LispStruct> tempSharpEqualReplTable = context.getSharpEqualReplTable();

		try {
			context.clearSharpEqualTables();

			return readPreservingWhitespace(inputStreamStruct, eofErrorP, eofValue, true);
		} finally {
			context.restoreSharpEqualTables(tempSharpEqualFinalTable, tempSharpEqualTempTable, tempSharpEqualReplTable);
		}
	}
}

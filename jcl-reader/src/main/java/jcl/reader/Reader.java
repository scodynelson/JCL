/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import java.util.Map;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.ReadCharResult;
import jcl.reader.internal.ReaderProcessor;
import lombok.experimental.UtilityClass;

/**
 * JCL Reader that handles reading in lisp tokens and parsing them as {@link LispStruct}s.
 */
@UtilityClass
public final class Reader {

	public static LispStruct read(final InputStreamStruct inputStreamStruct,
	                              final boolean eofErrorP,
	                              final LispStruct eofValue,
	                              final boolean recursiveP) {
		final LispStruct token = readPreservingWhitespace(inputStreamStruct, eofErrorP, eofValue, recursiveP);

		final ReadCharResult possibleWhitespace = inputStreamStruct.readChar(false, eofValue);
		final Integer codePoint = possibleWhitespace.getResult();
		if (!possibleWhitespace.isEof() && (!Character.isWhitespace(codePoint) || recursiveP)) {
			inputStreamStruct.unreadChar(codePoint);
		}

		return token;
	}

	public static LispStruct readPreservingWhitespace(final InputStreamStruct inputStreamStruct,
	                                                  final boolean eofErrorP,
	                                                  final LispStruct eofValue,
	                                                  final boolean recursiveP) {
		if (recursiveP) {
			return ReaderProcessor.read(inputStreamStruct, eofErrorP, eofValue);
		}

		final ReaderContext context = ReaderContextHolder.getContext();
		final Map<Integer, LispStruct> tempSharpEqualFinalTable = context.getSharpEqualFinalTable();
		final Map<Integer, SymbolStruct> tempSharpEqualTempTable = context.getSharpEqualTempTable();
		final Map<SymbolStruct, LispStruct> tempSharpEqualReplTable = context.getSharpEqualReplTable();

		try {
			context.clearSharpEqualTables();

			return readPreservingWhitespace(inputStreamStruct, eofErrorP, eofValue, true);
		} finally {
			context.restoreSharpEqualTables(tempSharpEqualFinalTable, tempSharpEqualTempTable, tempSharpEqualReplTable);
		}
	}
}

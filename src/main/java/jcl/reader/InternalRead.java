/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.lang.BooleanStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class InternalRead {

	public static LispStruct read(final InputStreamStruct inputStreamStruct,
	                              final BooleanStruct eofErrorP,
	                              final LispStruct eofValue,
	                              final BooleanStruct recursiveP) {

		return read(inputStreamStruct, eofErrorP.toJavaPBoolean(), eofValue, recursiveP.toJavaPBoolean());
	}

	public static LispStruct read(final InputStreamStruct inputStreamStruct,
	                              final boolean eofErrorP,
	                              final LispStruct eofValue,
	                              final boolean recursiveP) {

		try {
			return Reader.read(inputStreamStruct, eofErrorP, eofValue, recursiveP);
		} finally {
			ReaderContextHolder.clearContext();
		}
	}

	public static LispStruct readPreservingWhitespace(final InputStreamStruct inputStreamStruct,
	                                                  final BooleanStruct eofErrorP,
	                                                  final LispStruct eofValue,
	                                                  final BooleanStruct recursiveP) {

		return readPreservingWhitespace(inputStreamStruct, eofErrorP.toJavaPBoolean(), eofValue,
		                                recursiveP.toJavaPBoolean());
	}

	public static LispStruct readPreservingWhitespace(final InputStreamStruct inputStreamStruct,
	                                                  final boolean eofErrorP,
	                                                  final LispStruct eofValue,
	                                                  final boolean recursiveP) {

		try {
			return Reader.readPreservingWhitespace(inputStreamStruct, eofErrorP, eofValue, recursiveP);
		} finally {
			ReaderContextHolder.clearContext();
		}
	}
}

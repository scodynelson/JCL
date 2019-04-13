package jcl.reader;

import jcl.lang.BooleanStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;

public class InternalRead {

	private final Reader reader;

	public InternalRead(final Reader reader) {
		this.reader = reader;
	}

	public LispStruct read(final InputStreamStruct inputStreamStruct, final BooleanStruct eofErrorP, final LispStruct eofValue,
	                       final BooleanStruct recursiveP) {

		return read(inputStreamStruct, eofErrorP.toJavaPBoolean(), eofValue, recursiveP.toJavaPBoolean());
	}

	public LispStruct read(final InputStreamStruct inputStreamStruct, final boolean eofErrorP, final LispStruct eofValue,
	                       final boolean recursiveP) {

		try {
			return reader.read(inputStreamStruct, eofErrorP, eofValue, recursiveP);
		} finally {
			ReaderContextHolder.clearContext();
		}
	}

	public LispStruct readPreservingWhitespace(final InputStreamStruct inputStreamStruct, final BooleanStruct eofErrorP, final LispStruct eofValue,
	                                           final BooleanStruct recursiveP) {

		return readPreservingWhitespace(inputStreamStruct, eofErrorP.toJavaPBoolean(), eofValue, recursiveP.toJavaPBoolean());
	}

	public LispStruct readPreservingWhitespace(final InputStreamStruct inputStreamStruct, final boolean eofErrorP, final LispStruct eofValue,
	                                           final boolean recursiveP) {

		try {
			return reader.readPreservingWhitespace(inputStreamStruct, eofErrorP, eofValue, recursiveP);
		} finally {
			ReaderContextHolder.clearContext();
		}
	}
}

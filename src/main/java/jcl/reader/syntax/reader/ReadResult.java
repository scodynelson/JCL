package jcl.reader.syntax.reader;

import jcl.LispStruct;

public class ReadResult {

	private final Integer result;
	private final LispStruct eofValue;
	private final boolean wasEOF;

	public ReadResult(final Integer result) {
		this.result = result;
		eofValue = null;
		wasEOF = false;
	}

	public ReadResult(final LispStruct eofValue) {
		result = null;
		this.eofValue = eofValue;
		wasEOF = true;
	}

	public Integer getResult() {
		return result;
	}

	public LispStruct getEofValue() {
		return eofValue;
	}

	public boolean wasEOF() {
		return wasEOF || (result == null);
	}
}

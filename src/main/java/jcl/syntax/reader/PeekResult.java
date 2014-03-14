package jcl.syntax.reader;

import jcl.LispStruct;

public class PeekResult {

	private final Integer result;
	private final LispStruct eofValue;
	private final boolean wasEOF;

	public PeekResult(final Integer result) {
		this.result = result;
		eofValue = null;
		wasEOF = false;
	}

	public PeekResult(final LispStruct eofValue) {
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

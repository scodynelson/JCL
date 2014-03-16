package jcl.readtables.reader;

import jcl.LispStruct;
import jcl.readtables.LispReader;
import jcl.readtables.ReadtableStruct;
import jcl.readtables.TokenBuilder;
import jcl.streams.InputStream;
import jcl.syntax.CaseSpec;
import jcl.syntax.reader.ReadResult;
import jcl.variables.ReadtableVariable;

public class StateReader implements LispReader {

	private final InputStream inputStream;
	private final ReadtableStruct readtable;

	public StateReader(final InputStream inputStream) {
		this(inputStream, ReadtableVariable.INSTANCE.getValue());
	}

	public StateReader(final InputStream inputStream, final ReadtableStruct readtable) {
		this.inputStream = inputStream;
		this.readtable = readtable;
	}

	@Override
	public LispStruct read() {
		return read(true, null, true);
	}

	@Override
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final StateReader stateReader = new StateReader(inputStream);

		final TokenBuilder tokenBuilder = new TokenBuilder(eofErrorP, eofValue, recursiveP);
		InitialState.INITIAL_STATE.process(stateReader, tokenBuilder);

		return tokenBuilder.getReturnToken();
	}

	@Override
	public ReadResult readChar() {
		return readChar(true, null, true);
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStream.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public void unreadChar(final int codePoint) {
		inputStream.unreadChar(codePoint);
	}

	public ReadtableStruct getReadtable() {
		return readtable;
	}

	public CaseSpec getReadtableCase() {
		return readtable.getReadtableCase();
	}
}

package jcl.readtables.reader.impl.states.impl;

import jcl.LispStruct;
import jcl.readtables.ReadtableStruct;
import jcl.readtables.ReadtableVariable;
import jcl.readtables.reader.impl.states.StateReader;
import jcl.readtables.reader.impl.states.TokenBuilder;
import jcl.streams.InputStream;
import jcl.syntax.CaseSpec;
import jcl.syntax.reader.ReadResult;

public class StateReaderImpl implements StateReader {

	private final InputStream inputStream;
	private final ReadtableStruct readtable;

	public StateReaderImpl(final InputStream inputStream) {
		this(inputStream, ReadtableVariable.INSTANCE.getValue());
	}

	public StateReaderImpl(final InputStream inputStream, final ReadtableStruct readtable) {
		this.inputStream = inputStream;
		this.readtable = readtable;
	}

	@Override
	public LispStruct read() {
		return read(true, null, true);
	}

	@Override
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final StateReaderImpl stateReader = new StateReaderImpl(inputStream);

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

	@Override
	public ReadtableStruct getReadtable() {
		return readtable;
	}

	@Override
	public CaseSpec getReadtableCase() {
		return readtable.getReadtableCase();
	}
}
